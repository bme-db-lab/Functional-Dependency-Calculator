:- module(timeout, [reply_with_timeout/2, call_with_timeout/5, call_with_timeout/6]).
:- use_module(functional).

% predicate for timeout constant (in seconds)
timeout(1).

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
  timeout(T), % using the _small_ timeout constant
  catch(
    call_with_time_limit(
      T,
      (
        call(PredNormal, A, Solution0),
        map(Solution0, PredMapping, Solution1),
        %atomic_list_concat(Solution1, '~n', Solution2),
        Solution = Solution1
      )
    ),
    time_limit_exceeded, % exception type
    catch( % catch branch -- inner catch
      call_with_time_limit( % a possible timeout again
        T,
        (
          call(PredTimeout, A, Solution0),
          call(PredMapping, Solution0, Solution1),
          atomic_list_concat([Solution1], '~n', Solution2),
          string_concat('[timeout, only listing the first solution]~n', Solution2, Solution)
        )
      ),
      time_limit_exceeded, % exception type
      Solution = '[timeout]' % catch branch
    )
  ).

% call functions with two arguments
call_with_timeout(A, B, Solution, PredNormal, PredTimeout, PredMapping) :-
  timeout(T), % using the _small_ timeout constant
  catch(
    call_with_time_limit(
      T,
      (
        call(PredNormal, A, B, Solution0),
        map(Solution0, PredMapping, Solution1),
        %atomic_list_concat(Solution1, '~n', Solution2),
        Solution = Solution1
      )
    ),
    time_limit_exceeded, % exception type
    catch( % catch branch -- inner catch
      call_with_time_limit( % a possible timeout again
        T,
        (
          call(PredTimeout, A, B, Solution0),
          call(PredMapping, Solution0, Solution1),
          atomic_list_concat([Solution1], '~n', Solution2),
          string_concat('[timeout, only listing the first solution]~n', Solution2, Solution)
        )
      ),
      time_limit_exceeded, % exception type
      Solution = '[timeout]' % catch branch
    )
  ).

