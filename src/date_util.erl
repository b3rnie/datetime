%%%_* Module declaration ===============================================
-module(date_util).

-export([ next_day/1
        , prev_day/1
        , next_non_weekend/1
        , gaussian_between/2
        ]).


next_day(Date) -> loop(Date, +1).
prev_day(Date) -> loop(Date, -1).

next_non_weekend(Date0) ->
  {{Y,M,D}, _} = Date = next_day(Date0),
  case calendar:day_of_the_week(Y,M,D) of
    6 -> next_day(next_day(Date));
    7 -> next_day(Date);
    _ -> Date
  end.

gaussian_between(D0, D1) ->
  G0    = calendar:datetime_to_gregorian_seconds(D0),
  G1    = calendar:datetime_to_gregorian_seconds(D1),
  GSecs = round((G1-G0) * gaussian:zero_to_one()),
  calendar:gregorian_seconds_to_datetime(G0 + GSecs).

%%%_ * Internals -------------------------------------------------------
loop({Ymd0,Hms}, N) ->
  Ymd = next(Ymd0, N),
  case calendar:valid_date(Ymd) of
    true  -> {Ymd,Hms};
    false -> loop({Ymd,Hms},N)
  end.

next({Y,M,0} , _N) -> {Y,M-1,31};
next({Y,M,32}, _N) -> {Y,M+1,1};
next({Y,0,_},  _N) -> {Y-1,12,31};
next({Y,13,_}, _N) -> {Y+1,1,1};
next({Y,M,D},   N) -> {Y,M,D+N}.


%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


next_prev_test() ->
  ToGSecs = fun calendar:datetime_to_gregorian_seconds/1,
  Now     = calendar:local_time(),
  End     = lists:foldl(
              fun(_, {Yesterday0,Tomorrow0}) ->
                  Yesterday = prev_day(Yesterday0),
                  Tomorrow  = next_day(Tomorrow0),
                  Diff0 = ToGSecs(Yesterday0) - ToGSecs(Yesterday),
                  Diff1 = ToGSecs(Tomorrow)  - ToGSecs(Tomorrow0),
                  true = Diff0 >= 86400 - 10,
                  true = Diff0 =< 86400 + 10,
                  true = Diff1 >= 86400 - 10,
                  true = Diff1 =< 86400 + 10,
                  {Yesterday, Tomorrow}
              end, {Now,Now}, lists:seq(1, 1000)),
  io:format("End: ~p~n", [End]),
  true.

%% TODO: Fix these and make them actually check return values
gaussian_between_test() ->
  Now      = calendar:local_time(),
  Tomorrow = next_day(Now),
  gaussian_between(Now, Tomorrow),
  true.

next_non_weekend_test() ->
  next_non_weekend(calendar:local_time()),
  true.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

