-module(test_support_app@foreign).

-export([startImpl/1
       , stopImpl/1
       , setConfigImpl/3 ]).


startImpl(App) ->
  fun() ->
    application:ensure_all_started(App)
  end.

stopImpl(App) ->
  fun() ->
    application:stop(App)
  end.

setConfigImpl(App, Name, Value) ->
  fun() ->
    gproc:set_env(l, App, Name , Value, [ app_env ])
  end.
