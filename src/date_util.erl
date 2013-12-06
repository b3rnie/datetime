%%%_* Module declaration ===============================================
-module(date_util).

-export([ next_day/1
        , prev_day/1
        , next_day_non_weekend/1
        , gaussian_between/2
        , uniform_between/2
        ]).

next_day(Date) -> day_loop(Date, fun day_inc/1).
prev_day(Date) -> day_loop(Date, fun day_dec/1).

next_day_non_weekend(Date0) ->
  {{Y,M,D}, _} = Date = next_day(Date0),
  case calendar:day_of_the_week(Y,M,D) of
    6 -> next_day(next_day(Date));
    7 -> next_day(Date);
    _ -> Date
  end.

gaussian_between(D0, D1) -> between(D0, D1, fun gaussian:zero_to_one/0).
uniform_between(D0, D1)  -> between(D0, D1, fun random:uniform/0).

%%%_ * Internals -------------------------------------------------------
day_loop({Ymd0,Hms}, F) ->
  Ymd = F(Ymd0),
  case calendar:valid_date(Ymd) of
    true  -> {Ymd,Hms};
    false -> day_loop({Ymd,Hms}, F)
  end.

day_inc({Y, M,  31}) -> {Y,   M+1, 1  };
day_inc({Y, 13, _ }) -> {Y+1, 1,   1  };
day_inc({Y, M,  D }) -> {Y,   M,   D+1}.

day_dec({Y, M,  1 }) -> {Y,   M-1, 31 };
day_dec({Y, 0,  _ }) -> {Y-1, 12,  31 };
day_dec({Y, M,  D }) -> {Y,   M,   D-1}.

between(D0, D1, F) ->
  G0    = calendar:datetime_to_gregorian_seconds(D0),
  G1    = calendar:datetime_to_gregorian_seconds(D1),
  GSecs = round((G1-G0) * F()),
  calendar:gregorian_seconds_to_datetime(G0 + GSecs).

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
  next_day_non_weekend(calendar:local_time()),
  true.

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
