:- module(timeout, [timeout/1, reply_with_timeout/2, call_with_timeout/5, call_with_timeout/6]).
:- use_module(functional).

% predicate for timeout constant (in seconds)
timeout(2).
small_timeout(0.1).

% ========== timeout ==========

% simple request with timeout
reply_with_timeout(Request, Pred) :-
  timeout(T),
  catch(
    call_with_time_limit(T, (call(Pred, Request, Reply), HadTimeout = false)),
    time_limit_exceeded, % exception type
    (HadTimeout = true) % catch branch
  ),
  ( HadTimeout = true -> Reply = '[timeout]~n'
  ; true
  ),
  reply(Reply).

% call functions with one argument, fallback to single solution
call_with_timeout(A, Solution, PredNormal, PredTimeout, PredMapping) :-
  small_timeout(T), % using the _small_ timeout constant
  catch(
    call_with_time_limit(
      T,
      (
        call(PredNormal, A, Solution0),
        map(Solution0, PredMapping, Solution1),
        Solution = Solution1
      )
    ),
    time_limit_exceeded, % exception type
    ( % catch branch
      call(PredTimeout, A, Solution0),
      call(PredMapping, Solution0, Solution1),
      Solution2 = [Solution1],
      Solution = ['[timeout, only listing the first solution]'|Solution2],
      !
    )
  ).

% call functions with two arguments
call_with_timeout(A, B, Solution, PredNormal, PredTimeout, PredMapping) :-
  small_timeout(T), % using the _small_ timeout constant
  catch(
    call_with_time_limit(
      T,
      (
        call(PredNormal, A, B, Solution0),
        map(Solution0, PredMapping, Solution1),
        Solution = Solution1
      )
    ),
    time_limit_exceeded, % exception type
    ( % catch branch
      call(PredTimeout, A, B, Solution0),
      call(PredMapping, Solution0, Solution1),
      Solution2 = [Solution1],
      Solution = ['[timeout, only listing the first solution]'|Solution2],
      !
    )
  ).

