-module(blatus_main@foreign).

-export([ seed/0
        , random/2
        ]).

seed() ->
 rand:seed_s(exsss).

random(Fn, S0) ->
  {R1, S1 } = rand:uniform_s(S0),
  (Fn(R1))(S1).

