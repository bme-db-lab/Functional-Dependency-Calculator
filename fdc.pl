:- module(fdc, [cSingleRightSide/2, cNF/3, cFmin/2, cFequiv/2, cKeys/3, cPrimaryAttributes/3, cSecondaryAttributes/3, cFclose/3, cBCNF/3, c3NF/3, cDecomposeTo3NF/3]).
:- use_module(functional).
:- use_module(sets).
:- dynamic(leftred/1).  
:- dynamic(minimal/1).  
:- dynamic(bcnfdecomposition/1).
:- dynamic(d3nfdecomposition/1).

% operator for readable FDs
:- op(800, xfx, ->).

% the code relies heavily on backtracking, so it's worth noting
% that the -> operator destroyes choice-points created inside the clause.
% so instead of 
% ( Condition -> Then
% ; Else
% )
% we have to use
% ( Condition, "Then"...
% ; \+ Condition, "Else"...
% )
% to let backtrack work.
% see also: http://www.swi-prolog.org/pldoc/man?predicate=send_arrow%2f2

% XClosed = X+(F)
cClose(X, F, XClosed) :-
  ( X = [] -> XClosed = []
  ; foldr(F, fdc:cExpand, X, X0),
    ( X = X0 -> XClosed = X0
    ; cClose(X0, F, XClosed)
    )
  ).

% FD, set of attributes, expanded set of attributes
cExpand(Y->B, X, XExpanded) :-
  ( subset(Y, X) -> union(X, [B], XExpanded) % Y is a subset of X
  ; XExpanded = X % cannot expand
  ).

% X is a superkey if X+(F) = R
% two sets are equivalent if both are a subset of the other.
% the X+(F) is always a subset of R, so we need to check if R is a subset of X+(F) 
cSuperkey(R, F, X) :-
  cClose(X, F, XClosed), subset(R, XClosed).

% X->A is trivial if A is an element of X
cTrivial(X->A) :-
  memberchk(A, X).

% S is the superkeys of schema R with FDs F
cSuperKeys(R, F, S) :-
  powerSet(R, Hatv),
  findall(X, (member(X, Hatv), cSuperkey(R, F, X)), S).

% a key K is minimal if no real subset of K is a superkey
cMinimal(X, SuperKeys) :-
  \+ bagof(K, (member(K, SuperKeys), K \= X, subset(K, X)), _).
%             ^ looking for real subsets of X in the set of SuperKeys
% if we found one, the superkey is not minimal -> the negation causes the clause to fail

% key: 1) superkey 2) minimal
cKeys(R, F, Keys) :-
  cSuperKeys(R, F, SuperKeys),
  findall(X, (member(X, SuperKeys), cMinimal(X, SuperKeys)), Keys).

% set of primary attributes
cPrimaryAttributes(R, F, PrimaryAttributes) :-
  cKeys(R, F, Keys),
  union(Keys, PrimaryAttributes).
  
% set of secondary attributes
cSecondaryAttributes(R, F, SecondaryAttributes) :-
  cPrimaryAttributes(R, F, PrimaryAttributes),
  subtract(R, PrimaryAttributes, SecondaryAttributes).
  
% is primary attribute
cPrimary(R, F, A) :-
  cPrimaryAttributes(R, F, PrimaryAttributes),
  memberchk(A, PrimaryAttributes).
  
% ==================== BCNF ===================
% for all X->A in we check if it satisfies BCNF
cTestBCNF(R, F) :-
  findall(XA, (member(XA, F), cSatisfiesBCNF(R, F, XA)), L),
  subset(F, L).

% X->A non-trivial FD satisfies BCNF if X is a superkey
cSatisfiesBCNF(R, F, X->A) :-
  ( cTrivial(X->A)
  ; cSuperkey(R, F, X)
  ).

% ==================== 3NF ====================
cTest3NF(R, F) :-
  findall(XA, (member(XA, F), cSatisfies3NF(R, F, XA)), L),
  subset(F, L).

% X->A non-trivial FD satisfies 3NF if X is a superkey or A is a primary attribute
cSatisfies3NF(R, F, X->A) :- 
  ( cTrivial(X->A)
  ; cSuperkey(R, F, X)
  ; cPrimary(R, F, A)
  ).
  
% ==================== 2NF ==================== 
cTest2NF(R, F) :-
  cKeys(R, F, Keys),
  cSecondaryAttributes(R, F, SecondaryAttributes),
  % collect the solutions of key->secondary attribute FDs
  \+ bagof(K->A, (member(K, Keys), member(A, SecondaryAttributes), \+ cSatisfies2NF(F, K->A)), _).

% K->A (where K is a key, A is a secondary attribute) satisfies 2NF if no real subset X of K exist such that X->A
cSatisfies2NF(F, K->A) :-
  powerSet(K, KSubsets),
  subtract(KSubsets, [K], KRealSubsets),
  \+ bagof(X, (member(X, KRealSubsets), cClose(X, F, XClosed), memberchk(A, XClosed)), _).

% ============ highest normal form =============
cNF(R, F, NF) :-
  ( cTestBCNF(R, F) -> NF = nfBCNF
  ; cTest3NF(R, F)  -> NF = nf3NF
  ; cTest2NF(R, F)  -> NF = nf2NF
  ; NF = nf1NF
  ).

% 1st step of minimalizing
%   all FDs may have a single attribute on their right side
cSingleRightSide(F, FFormatted) :-
  foldl(F, fdc:cDecompose, [], F0),
  lists:reverse(F0, FFormatted).

% decomposing right side of a FD (consequence of Armstrong's axioms)
cDecompose(X->Y, F, F1) :-
  ( Y = [A|Yt] -> cDecompose(X->Yt, [X->A|F], F1)
  ; F1 = F
  ).

% 2nd step of minimalizing
%   omitting superfluous attributes from the left side of FDs
cMinimalizeLeftSide(F, FLeftRed) :-
  retractall(leftred(_)),
  cMinimalizeLeftSide(F, F, FLeftRed, false).

cMinimalizeLeftSide([], FLeftRed, FLeftRed, false) :-
  \+ leftred(FReduced), assert(leftred(FReduced)).
cMinimalizeLeftSide([X->A|T], F, FLeftRed, SkippedFlag) :-
  ( cReducible(X->A, F, Y),
    subtract(F, [X->A], F0),
    union(F0, [Y->A], F1),
    ( cMinimalizeLeftSide(F1, FLeftRed)
    ; cMinimalizeLeftSide(T, F, FLeftRed, true)
    )
  ; \+ cReducible(X->A, F, Y), cMinimalizeLeftSide(T, F, FLeftRed, SkippedFlag)
  ).

% 3rd step of minimalizing
%   skipping deducible FDs
cSkipFDs(F, FMin) :-
  cSkipFDs(F, F, FMin, false).
  
% we may skip X->A if A is in (X)+(G), where G is F \ {X->A}
% cSkipFDs(tail of FDs, all FDs, minimalised FDs, reduced bit)
cSkipFDs([], FReduced, FReduced, false).
cSkipFDs([X->A|T], F, FReduced, SkippedFlag) :-
  subtract(F, [X->A], G),                    % G = F \ {X->A}
  cClose(X, G, XClosed),                     % calculating X+(G)
  ( memberchk(A, XClosed) ->                 % A is in X+(G)
    ( cSkipFDs(G, FReduced)
    ; cSkipFDs(T, F, FReduced, true)
    )
  ; cSkipFDs(T, F, FReduced, SkippedFlag)
  ).
  
cReducible(X->A, F, Y) :-
  cLeftRed(X, X->A, F, Y).
  
cLeftRed([H|T], X->A, F, Y) :-
  subtract(X, [H], X0),
  cClose(X0, F, X0C),
  ( memberchk(A, X0C), Y = X0
  ; cLeftRed(T, X->A, F, Y)
  ).  

cFmin(F, FMin) :-
  retractall(minimal(_)),
  cSingleRightSide(F, F1),
  cMinimalizeLeftSide(F1, F2),
  cSkipFDs(F2, F3),
  sort(F3, FMin),
  \+ minimal(FMin), assert(minimal(FMin)).

cFsubset([], _G).
cFsubset([X->A|T], G) :-
  cClose(X, G, XClosed),
  memberchk(A, XClosed),
  cFsubset(T, G).

cFequiv(F, G) :-
  cFsubset(F, G),
  cFsubset(G, F).

cNonTrivialClosures([], _, []).
cNonTrivialClosures([H|T], F, FClosed) :-
  cNonTrivialClosures(T, F, TClosed),
  cClose(H, F, HClosed),
  subtract(HClosed, H, HClosedNonTrivial),
  ( HClosedNonTrivial = [] -> FClosed = TClosed
  ; FClosed = [H->HClosedNonTrivial|TClosed]
  ).

cFclose(R, F, FClosed) :-
  powerSet(R, RP),
  cNonTrivialClosures(RP, F, FClosed0),
  cSingleRightSide(FClosed0, FClosed).

cFilterProjected([], _, []).
cFilterProjected([X->A|T], S, FF) :-
  cFilterProjected(T, S, TFF),
  ( memberchk(A, S) -> FF = [X->A|TFF]
  ; FF = TFF
  ).
  
% FP is the set of projected FDs on F 
cProjectFDs(F, S, FP) :-
  cFclose(S, F, FP0),
  cFilterProjected(FP0, S, FP).

% enumerate the next BCNF decomposition
cBCNF(S, G, Rho) :-
  retractall(bcnfdecomposition(_)),
  cDecomposeToBCNF(S, G, Rho0),
  sort(Rho0, Rho),
  \+ bcnfdecomposition(Rho),
  assert(bcnfdecomposition(Rho)).

% decompose to BCNF
cDecomposeToBCNF(S, G, Rho) :-  
  findall(XA, (member(XA, G), cSatisfiesBCNF(S, G, XA)), SatisfyingBCNF),
  subtract(G, SatisfyingBCNF, ViolatingBCNF), % find the FDs that violate the BCNF property
  ( member(X->A, ViolatingBCNF),
    (
      union(X, [A], S1),
      subtract(S, [A], S2),
      cProjectFDs(G, S1, G1),
      cProjectFDs(G, S2, G2),
      sort(S1, S10),
      sort(S2, S20),
      cDecomposeToBCNF(S10, G1, Rho1),
      cDecomposeToBCNF(S20, G2, Rho2),
      append(Rho1, Rho2, Rho)
    )
  ; ViolatingBCNF = [], Rho = [S]
  ).

% enumerate the next BCNF decomposition
c3NF(S, G, Rho) :-
  retractall(d3nfdecomposition(_)),
  cDecomposeTo3NF(S, G, Rho0),
  sort(Rho0, Rho),
  \+ d3nfdecomposition(Rho),
  assert(d3nfdecomposition(Rho)).

% decompose to 3NF
cDecomposeTo3NF(S, G, Rho) :-
  cSingleRightSide(G, G0),
  cKeys(S, G0, Keys),                       % determine the keys
  member(Key, Keys),                        % pick a key
  cFmin(G, GMin),                           % determine a possible minimal FD set
  map(GMin, fdc:cDependencyToSchema, Rho0), % convert the minimal FD set to relational schemas
  ( hasSuperSet(Rho0, Key) -> Rho = Rho0    % it contains the key, there is no need to add 
  ; union(Rho0, [Key], Rho1), sort(Rho1, Rho)
  ).

% converts a (canonical) minimal FD to relational schema,
% e.g. [a, b]->e becomes [a, b, e]
cDependencyToSchema(X->Y, R) :-
  union(X, [Y], R0),
  sort(R0, R).

% returns if there is a set in a list of sets L which is a superset of S
hasSuperSet(L, S) :-
  member(E, L), !,
  ( call(subset, S, E), !
  ; L = [_|T], hasSuperSet(T, S)
  ).

