:- module(fd, [nf/3, fmin/2, fequiv/2, keys/3, primaryattributes/3, secondaryattributes/3, fclose/3, bcnf/3, bcnfs/3, d3nf/3, d3nfs/3]).
:- use_module(functional).
:- use_module(fdc).
:- use_module(library(lists)).
:- use_module(library(time)).

% convert atom to list of one-character atoms and backwards
% myatom <---> [m, y, a, t, o, m]
%    [a] ----> a
%     a  <---- a
atom_to_list(A, L) :- 
  ( atom(L) -> A = L
  ; atom_chars(A, L)
  ).

list_of_atom_to_list([], []).
list_of_atom_to_list([A|As], [L|Ls]) :-
  atom_to_list(A, L),
  list_of_atom_to_list(As, Ls).

list_sort([], []).
list_sort([H|T], [HS|TS]) :-
  sort(H, HS),
  list_sort(T, TS).

% from pretty to canonical
prettyFDs(FC, FP) :-
  map(FC, fd:prettyFD, FP).

prettyFD(XL->YL, XA->YA) :-
  atom_to_list(XA, XL),
  atom_to_list(YA, YL).
  
% from canonical to pretty
canonicalFDs(FC, FP) :-
  map(FC, fd:canonicalFD, FP).

canonicalFD(C, P) :-
  prettyFD(P, C).
  
nf(R, F, N) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cNF(R0, F1, N).
  
fmin(F, Fmin) :-
  canonicalFDs(F, F0),
  cFmin(F0, F1),
  prettyFDs(F1, Fmin).

fequiv(F, G) :-
  canonicalFDs(F, F0),
  canonicalFDs(G, G0),
  cSingleRightSide(F0, F1),
  cSingleRightSide(G0, G1),
  cFequiv(F1, G1).

keys(R, F, Keys) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cKeys(R0, F1, Keys0),
  list_of_atom_to_list(Keys, Keys0).

primaryattributes(R, F, PrimaryAttributes) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cPrimaryAttributes(R0, F1, PrimaryAttributes0),
  sort(PrimaryAttributes0, PrimaryAttributes1),
  atom_to_list(PrimaryAttributes, PrimaryAttributes1).

secondaryattributes(R, F, SecondaryAttributes) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cSecondaryAttributes(R0, F1, SecondaryAttributes0),
  sort(SecondaryAttributes0, SecondaryAttributes1),
  atom_to_list(SecondaryAttributes, SecondaryAttributes1).

fclose(R, F, FClosed) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cFclose(R0, F1, FClosed0),
  prettyFDs(FClosed0, FClosed).

% decomposition to BCNF schemes
bcnf(R, F, Rho) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cBCNF(R0, F1, Rho0),
  list_of_atom_to_list(Rho, Rho0).

% bcnfs(abcde, [ab->cd, b->e, d->e], Rhos).
% bcnf(itkoscmpd, [it->k, t->oscm, cm->pd, p->c], Rhos).

% convert a scheme decomposition (set of atoms) to readable text
decomposition_to_text(L, L0) :-
  atomic_list_concat(L, ', ', L0).

% decomposition to 3NF schemes
d3nf(R, F, Rho) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  c3NF(R0, F0, Rho0),
  list_of_atom_to_list(Rho, Rho0).

% aggregate all BCNF decompositions
bcnfs_all(R, F, Rhos) :-
  findall(Rho, bcnf(R, F, Rho), Rhos).

bcnfs(R, F, Rhos) :-
  call_with_time_limit(1, bcnfs_all(R, F, Rhos0)),
  map(Rhos0, fd:decomposition_to_text, Rhos1),
  atomic_list_concat(Rhos1, '~n', Rhos).

% aggregate all 3NF decompositons
d3nfs_all(R, F, Rhos) :-
  findall(Rho, d3nf(R, F, Rho), Rhos).

d3nfs(R, F, Rhos) :-
  call_with_time_limit(1, d3nfs_all(R, F, Rhos0)),
  map(Rhos0, fd:decomposition_to_text, Rhos1),
  atomic_list_concat(Rhos1, '~n', Rhos).

