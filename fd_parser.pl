:- module(fd_parser, [parse_fds/2, fds_to_string/2]).
:- use_module(functional).

% operator for readable FDs
:- op(800, xfx, ->).

strip_spaces(S, NoSpaces) :-
  atomic_list_concat(L, ' ', S),
  atomic_list_concat(L, NoSpaces).

fd_string_to_list(S, L) :-
  strip_spaces(S, NoSpaces),
  atomic_list_concat(L, ',', NoSpaces).

fds_to_string(L, S) :-
  map(L, fd_parser:fd_to_string, S0),
  atomic_list_concat(S0, ', ', S).

fd_to_string(X->Y, S) :-
  atomic_list_concat([X,Y], '->', S).

fdsplitter(S, FD) :-
  atomic_list_concat(FD0, '->', S),
  FD0 = [X,Y],
  FD = (X->Y).

parse_fds(S, FD) :-
  fd_string_to_list(S, L), map(L, fd_parser:fdsplitter, FD).

