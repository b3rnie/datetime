%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @copyright Bjorn Jensen-Urstad 2013
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(datetime).

%%%_* Exports ==========================================================
-export([ next_day/1
        , prev_day/1
        , next_day_non_weekend/1
        , random_date_between/3
        ]).

%%%_* Includes =========================================================
%%%_* Code =============================================================
%%%_ * API -------------------------------------------------------------
next_day(Date) -> day_loop(Date, fun day_inc/1).
prev_day(Date) -> day_loop(Date, fun day_dec/1).

next_day_non_weekend(Date0) ->
  {{Y,M,D}, _} = Date = next_day(Date0),
  case calendar:day_of_the_week(Y,M,D) of
    6 -> next_day(next_day(Date));
    7 -> next_day(Date);
    _ -> Date
  end.

random_date_between(D0, D1, gaussian) ->
  do_random_date_between(D0, D1, fun gaussian:zero_to_one/0);
random_date_between(D0, D1, uniform) ->
  do_random_date_between(D0, D1, fun random:uniform/0).

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

do_random_date_between(D0, D1, F) ->
  G0    = calendar:datetime_to_gregorian_seconds(D0),
  G1    = calendar:datetime_to_gregorian_seconds(D1),
  GSecs = round((G1-G0) * F()),
  calendar:gregorian_seconds_to_datetime(G0 + GSecs).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

next_day_test() ->
  T = {12,0,0},
  {{2013, 12, 1}, T} = next_day({{2013, 11, 30}, T}),
  {{2014, 1,  1}, T} = next_day({{2013, 12, 31}, T}),
  true.

-define(l_day, (86400 + 10)).
-define(s_day, (86400 - 10)).
next_prev_non_weekend_test() ->
  Now     = calendar:local_time(),
  End     = lists:foldl(
              fun(_, {Yesterday0,Tomorrow0,Weekend0}) ->
                  %% prev day
                  Yesterday = prev_day(Yesterday0),
                  Diff0 = to_gsecs(Yesterday0) - to_gsecs(Yesterday),
                  true = Diff0 > ?s_day,
                  true = Diff0 < ?l_day,

                  %% tomorrow
                  Tomorrow  = next_day(Tomorrow0),
                  Diff1 = to_gsecs(Tomorrow) - to_gsecs(Tomorrow0),
                  true = Diff1 > ?s_day,
                  true = Diff1 < ?l_day,

                  %% weekend
                  {{Y0,M0,D0}, _} = Weekend0,
                  {{Y,M,D},_}  = Weekend = next_day_non_weekend(Weekend0),
                  Diff3 = to_gsecs(Weekend) - to_gsecs(Weekend0),
                  true = calendar:day_of_the_week({Y,M,D}) =< 5,
                  case calendar:day_of_the_week(Y0,M0,D0) of
                    5 -> true = Diff3 > ?s_day*3,
                         true = Diff3 < ?l_day*3;
                    _ -> true = Diff3 > ?s_day,
                         true = Diff3 < ?l_day
                  end,
                  {Yesterday, Tomorrow, Weekend}
              end, {Now,Now,Now}, lists:seq(1, 1000)),
  io:format("End: ~p~n", [End]),
  true.

random_date_between_test() ->
  Now           = calendar:local_time(),
  Tomorrow      = next_day(Now),
  NowGSecs      = to_gsecs(Now),
  TomorrowGSecs = to_gsecs(Tomorrow),
  [begin
     D    = to_gsecs(random_date_between(Now, Tomorrow, Distribution)),
     true = D >= NowGSecs,
     true = D =< TomorrowGSecs
   end || _ <- lists:seq(1, 100), Distribution <- [uniform, gaussian]],
  true.

next_non_weekend_test() ->
  Friday = {{2013,12,6}, {12,0,0}},
  Sat    = {{2013,12,7}, {12,0,0}},
  Sun    = {{2013,12,8}, {12,0,0}},
  Mon    = {{2013,12,9}, {12,0,0}},
  Mon    = next_day_non_weekend(Friday),
  Mon    = next_day_non_weekend(Sat),
  Mon    = next_day_non_weekend(Sun),
  true.

to_gsecs(Date) ->
  calendar:datetime_to_gregorian_seconds(Date).

-else.
-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
