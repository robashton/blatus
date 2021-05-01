-module(test_main@foreign).
-export([ filterSasl/0
        ]).

filterSasl() ->
  fun () ->
      Level = case os:getenv("LOG") of
                false -> none;
                Val -> list_to_atom(Val)
              end,
      logger:set_primary_config(level, Level),
      unit
  end.
