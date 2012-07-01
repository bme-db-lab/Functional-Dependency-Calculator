:- module(fd_parser, [parse_fds/2]).

% operator for readable FDs
:- op(800, xfx, ->).

strip_spaces(S, NoSpaces) :-
  atomic_list_concat(L, ' ', S),
  atomic_list_concat(L, NoSpaces).

fd_to_list(S, L) :-
  strip_spaces(S, NoSpaces),
  atomic_list_concat(L, ',', NoSpaces).

map([X|Xs], Pred, [Y|Ys]) :-
  call(Pred, X, Y), map(Xs, Pred, Ys).
map([], _, []).

fdsplitter(S, FD) :-
  atomic_list_concat(FD0, '->', S),
  FD0 = [X,Y],
  FD = (X->Y).

parse_fds(S, FD) :-
  fd_to_list(S, L), map(L, fdsplitter, FD).

