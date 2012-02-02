:- module(fd, [nf/3, fmin/2, fequiv/2]).
:- use_module(functional).
:- use_module(fdc).
:- use_module(library(lists)).

% convert atom to list of one-character atoms and backwards
% myatom <---> [m, y, a, t, o, m]
%    [a] ----> a
%     a  <---- a
atom_to_list(A, L) :- 
  ( atom(L) -> A = L
  ; atom_chars(A, L)
  ).

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
