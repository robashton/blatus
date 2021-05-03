-module(erl_tests_eUnit_discovery@foreign).

-export([ findModuleNames/1
        , getExportedTests/1
        , filterTests/2
        ]).

findModuleNames(Directory) ->
  fun() ->
      Pattern = filename:join(Directory, <<"**/*.purs">>),
      lists:map(fun(Name) ->
                    { ok, H } = file:open(Name, [read]),
                    { ok, L } = file:read_line(H),
                    file:close(H),
                    Split = string:split(L, " ", all),
                    list_to_binary(string:trim(lists:nth(2, Split)))
                end,
       filelib:wildcard(binary_to_list(Pattern)))
  end.

getExportedTests(ModuleName) ->
  fun() ->
      case lists:member({tests,0}, ModuleName:module_info(exports)) of
        true -> {just, ModuleName:tests()};
        false -> {nothing}
      end
  end.

%% TODO: This is not exhaustive and if EUnit changes, this will need adding to
filterTests(FilterFn, []) ->
  [];
filterTests(FilterFn, [ H | Tail ]) ->
  case H of
    _Suite = { Name, SubTests } when is_binary(Name) and is_list(SubTests) ->
      [ { Name, filterTests(FilterFn, SubTests) } | filterTests(FilterFn, Tail) ];


    _SetupTeardown = { setup, Setup, Teardown, SubTests } ->
      [ { setup, Setup, Teardown, filterTests(FilterFn, SubTests) } | filterTests(FilterFn, Tail) ];

    _Spawn = { spawn, {Name, TestFn} } ->
      case FilterFn(Name) of
        true -> [{ spawn, { Name, TestFn } } | filterTests(FilterFn, Tail)];
        false ->
          filterTests(FilterFn, Tail)
      end
  end.
