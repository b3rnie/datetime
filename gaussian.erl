%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% Math copied from http://www.taygeta.com/random/gaussian.html
%%% @copyright Bjorn Jensen-Urstad 2013
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ===============================================
-module(gaussian).

%%%_* Exports ==========================================================
-export([basic/0, box_muller/0, zero_to_one/0, test/0]).

%%%_* Code =============================================================
%% Result is two new independent random numbers which have a
%% Gaussian distribution with zero mean and a standard deviation of one.
basic() ->
  X1 = random:uniform(),
  X2 = random:uniform(),
  Y1 = math:sqrt(-2.0 * math:log(X1)) * math:cos(2.0 * math:pi() * X2),
  Y2 = math:sqrt(-2.0 * math:log(X1)) * math:sin(2.0 * math:pi() * X2),
  {Y1, Y2}.

%% Result is two new independent random numbers which have a
%% Gaussian distribution with zero mean and a standard deviation of one.
box_muller() ->
  F = fun() ->
          X1 = 2.0 * random:uniform() - 1.0,
          X2 = 2.0 * random:uniform() - 1.0,
          W  = X1 * X1 + X2 * X2,
          {X1, X2, W}
      end,
  {X1, X2, W0} = until(fun({_,_,W}) -> W < 1.0 end, F),
  W  = math:sqrt((-2.0 * math:log(W0)) / W0),
  Y1 = X1 * W,
  Y2 = X2 * W,
  {Y1, Y2}.

zero_to_one() ->
  %% drops 0.3%
  {Y1, _} = until(fun({Y, _}) -> Y >= -3.0 andalso Y =< 3.0 end,
                  fun box_muller/0),
  (Y1 + 3) / 6.

%%%_ * Internals -------------------------------------------------------
until(F1, F2) ->
  Res = F2(),
  case F1(Res) of
    true  -> Res;
    false -> until(F1,F2)
  end.

test() ->
  L0 = [box_muller() || _ <- lists:seq(1, 10000)],
  L1 = lists:filter(fun({X1, _X2}) ->
                        X1 =< 1.0 andalso X1 >= -1.0
                    end, L0),
  L1.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
