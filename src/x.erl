-module(x).

-export([do/0]).

do() ->
  InitialModel = (profileSetup@ps:setup())(),
%%  { Time, _ } = timer:tc(fun() -> fprof:apply({profileSetup@ps, go}, [InitialModel], []) end),
  { Time, _ } = timer:tc(fun() -> profileSetup@ps:go(InitialModel) end),
  io:format(user, "Loops took ~p us ~n", [ Time ]),
%%  fprof:profile(),
%%  fprof:analyse(),
%%  eprof:profile(fun() -> profileSetup@ps:go(InitialModel) end),
 ok.
