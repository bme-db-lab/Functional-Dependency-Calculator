:- module(sets, [powerSet/2, subtract/3, union/3, union/2, subset/2]).

% from the Erlang equivalent in http://dp.iit.bme.hu/dp-current/dp11a-minden-p1.pdf
insAll(_, [], Zss, Zss).
insAll(X, [Ys|Yss], Zss, Xss) :-
  insAll(X, Yss, [[X|Ys]|Zss], Xss).

powerSet([], [[]]).
powerSet([X|Xs], PS) :-
  powerSet(Xs, P),
  insAll(X, P, P, PS), !.
  % we need the cut (!) because only one solution is possible
  % -> Prolog should not backtrack
  % (the procedure is translated from an Erlang function and Erlang does not backtrack)
  
% from SWI-Prolog's lists.pl
subtract([], _, []) :- !.
subtract([E|T], D, R) :-
	memberchk(E, D), !,
	subtract(T, D, R).
subtract([H|T], D, [H|R]) :-
	subtract(T, D, R).
  
union([], L, L) :- !.
union([H|T], L, R) :-
	memberchk(H, L), !,
	union(T, L, R).
union([H|T], L, [H|R]) :-
	union(T, L, R).
  
union(Sets, USet) :-
  union2(Sets, [], USet).

union2([], USetAcc, USetAcc).
union2([H|T], USetAcc, USet) :-
  union(USetAcc, H, USetAcc0),
  union2(T, USetAcc0, USet).

subset([], _) :- !.
subset([E|R], Set) :-
	memberchk(E, Set),
	subset(R, Set).
