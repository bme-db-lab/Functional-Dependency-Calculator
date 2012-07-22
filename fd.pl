:- module(fd, [nf/3, fmin/2, fmins_all/2, fequiv/2, keys/3, primaryattributes/3, secondaryattributes/3, fclose/3, bcnf/3, bcnfs_all/3, d3nf/3, d3nfs_all/3]).
:- use_module(functional).
:- use_module(fdc).
:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(fd_parser).

% convert atom to list of one-character atoms and backwards
% myatom <---> [m, y, a, t, o, m]
%    [a] ----> add
%     a  <---- a
atom_to_list(A, L) :- 
  ( atom(L) -> A = L
  ; atom_chars(A, L)
  ).

list_of_atom_to_list([], []) :- !.
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

% ========== ==========

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
  sort(PrimaryAttributes0, PrimaryAttributes).

secondaryattributes(R, F, SecondaryAttributes) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cSecondaryAttributes(R0, F1, SecondaryAttributes0),
  sort(SecondaryAttributes0, SecondaryAttributes).

% convert a schema decomposition (set of atoms) to readable text
decomposition_to_text(L, L0) :-
  atomic_list_concat(L, ', ', L0).

% calculate the closure of a set
fclose(R, F, FClosed) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cFclose(R0, F1, FClosed0),
  prettyFDs(FClosed0, FClosed).

% ==================== prodecures that return multiple answers ====================
% naming convention:
%  - proc: produces one answer, can backtrack (e.g. by hitting the ; key on the console)
%  - procs_all: produces all answers in a list
%  - procs: produces all answers in multiline text format

% ========= BCNF ==========
% decomposition to BCNF schemas
bcnf(R, F, Rho) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  cSingleRightSide(F0, F1),
  cBCNF(R0, F1, Rho0),
  list_of_atom_to_list(Rho, Rho0).

% list all BCNF decompositions
bcnfs_all(R, F, Rhos) :-
  findall(Rho, bcnf(R, F, Rho), Rhos).

% ========== 3NF ==========
% decomposition to 3NF schemas
d3nf(R, F, Rho) :-
  atom_to_list(R, R0),
  canonicalFDs(F, F0),
  c3NF(R0, F0, Rho0),
  list_of_atom_to_list(Rho, Rho0).

% aggregate all 3NF decompositons
d3nfs_all(R, F, Rhos) :-
  findall(Rho, d3nf(R, F, Rho), Rhos).

% ========= FMin ==========
fmin(F, Fmin) :-
  canonicalFDs(F, F0),
  cFmin(F0, F1),
  prettyFDs(F1, Fmin).

fmins_all(F, FMins) :-
  findall(FMin, fmin(F, FMin), FMins).


