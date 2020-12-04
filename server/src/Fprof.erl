-module(fprof@foreign).

-export([ start/0
        , stop/0
        ]).


start()  ->
  fun() ->
      fprof:trace(start)
  end.

stop() ->
  fun() ->
      fprof:trace(stop)
  end.
