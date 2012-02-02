:- module(functional, [foldl/4, foldr/4, map/3]).
% fold functions
foldl([X|Xs], Pred, Y0, Y) :-
  call(Pred, X, Y0, Y1), foldl(Xs, Pred, Y1, Y).
foldl([], _, Y, Y).

foldr([X|Xs], Pred, Y0, Y) :-
  foldr(Xs, Pred, Y0, Y1), call(Pred, X, Y1, Y).
foldr([], _, Y, Y).

% mapping function
map([X|Xs], Pred, [Y|Ys]) :-
  call(Pred, X, Y), map(Xs, Pred, Ys).
map([], _, []).