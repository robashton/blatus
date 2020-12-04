-module(pure_runningGameList@foreign).

-export([ generateId/0 ]).


generateId() ->
  fun() ->
      Bin = crypto:strong_rand_bytes(16),
      Length = size(Bin) * 8,
      <<Num:Length/big-unsigned-integer>> = Bin,
      list_to_binary(lists:flatten(io_lib:format("~*.16.0b", [size(Bin) * 2, Num])))
  end.
