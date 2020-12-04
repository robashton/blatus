-module(pure_timing@foreign).

-export([currentMs/0]).


currentMs() ->
  fun() ->
      erlang:system_time(millisecond)
  end.
