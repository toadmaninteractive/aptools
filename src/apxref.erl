-module(apxref).

%% Include files

%% Exported functions

-export([
    analyze_modules/1,
    analyze_modules/2,
    analyze_modules/3,
    analyze_applications/1,
    analyze_applications/2
]).

% https://www.graphviz.org/
% "c:/Program Files (x86)/Graphviz 2.28/bin/dot.exe"

%% API

analyze_modules(Applications) ->
    analyze_modules(all, Applications, default).

analyze_modules(Applications, Params) ->
    analyze_modules(all, Applications, Params).

-spec analyze_modules(Modules, Applications, Params) -> Calls when
    Modules :: [atom()] | 'all',
    Applications :: [atom()] | atom(),
    Params :: [Param] | Param,
    Param :: Modes | DotFileOptions | GraphvizOptions | 'default' | include_orphan_modules | verbose,
    Modes :: {'mode', ModeType | ModeTypes},
    ModeTypes :: [ModeType],
    ModeType :: 'export' | 'import',
    DotFileOptions :: 'make_dot_file' | {'dot_file_path', FilePath} | 'group_by_application',
    GraphvizOptions :: 'run_graphviz' | {'image_path', FilePath} | {'graphviz_path', FilePath},
    FilePath :: string(),
    Calls :: [{atom(), atom()}].

analyze_modules(Modules, Applications, InParams) ->
    Params = get_params(InParams),
    apxref_analyzer:start(get_applications(Applications)),
    InputModules = get_input_modules(Modules),
    Calls = get_module_calls(InputModules, get_mode_type(proplists:get_value(mode, Params))),

    %% dump .dot file
    FileName = proplists:get_value(dot_file_path, Params, "dump.dot"),
    AllModules = get_dump_modules(lists:member(include_orphan_modules, Params), Calls),
    AppModules = get_app_modules(lists:member(group_by_application, Params), AllModules),

    apxref_dumper:dump_xrefs(FileName, AppModules, AllModules, Calls),

    % run_graphviz
    case lists:member(run_graphviz, Params) of
        true ->
            ImagePath = proplists:get_value(image_path, Params, "out.png"),
            GraphvizPath = proplists:get_value(graphviz_path, Params, os:find_executable("dot.exe")),
            apxref_dumper:run_graphviz(GraphvizPath, FileName, ImagePath);
        false ->
            ignore
    end,

    apxref_analyzer:stop(),

    case lists:member(verbose, Params) of
        true -> Calls;
        false -> ok
    end.

analyze_applications(Applications) ->
    analyze_applications(Applications, default).

-spec analyze_applications(Applications, Params) -> Calls when
    Applications :: [atom()] | atom(),
    Params :: [Param] | Param,
    Param :: DotFileOptions | GraphvizOptions | 'default' | verbose,
    DotFileOptions :: 'make_dot_file' | {'dot_file_path', FilePath},
    GraphvizOptions :: 'run_graphviz' | {'image_path', FilePath} | {'graphviz_path', FilePath},
    FilePath :: string(),
    Calls :: [{atom(), atom()}].

analyze_applications(Applications, InParams) ->
    Params = get_app_params(InParams),
    apxref_analyzer:start(get_applications(Applications)),
    InputModules = get_input_modules(all),
    Calls = get_module_calls(InputModules, get_mode_type(proplists:get_value(mode, Params))),
    AppMods = apxref_analyzer:application_modules(),

    AppCalls0 = lists:map(fun({M1, M2}) -> {get_app_by_module(M1, AppMods), get_app_by_module(M2, AppMods)} end, Calls),
    AppCalls1 = lists:filter(fun({A1, A2}) -> A1 =/= A2 end, AppCalls0),
    AppCalls = lists:usort(AppCalls1),


    FileName = proplists:get_value(dot_file_path, Params, "dump.dot"),
    apxref_dumper:dump_xrefs(FileName, [], [], AppCalls),

    % run_graphviz
    case lists:member(run_graphviz, Params) of
        true ->
            ImagePath = proplists:get_value(image_path, Params, "out.png"),
            GraphvizPath = proplists:get_value(graphviz_path, Params, os:find_executable("dot.exe")),
            apxref_dumper:run_graphviz(GraphvizPath, FileName, ImagePath);
        false ->
            ignore
    end,

    apxref_analyzer:stop(),

    case lists:member(verbose, Params) of
        true -> Calls;
        false -> ok
    end.

%% Local functions

get_app_by_module(Module, AppModules) ->
    Res = lists:filter(fun({_Name, Mods}) -> lists:member(Module, Mods) end, AppModules),
    case Res of
        [] ->
            false;
        [{Name, _}] ->
            Name;
        _ ->
            io:format("Module ~w is not found in ~w~n", [Module, AppModules]),
            false
    end.


get_app_params(Params) when is_list(Params) ->
    case lists:member(default, Params) of
        true ->
            [make_dot_file, run_graphviz | Params];
        false ->
            Params
    end;
get_app_params(Param) ->
    get_app_params([Param]).

get_params(Params) when is_list(Params) ->
    case lists:member(default, Params) of
        true ->
            [make_dot_file, run_graphviz, group_by_application, include_orphan_modules | Params];
        false ->
            Params
    end;
get_params(Param) ->
    get_params([Param]).

get_applications(Atom) when is_atom(Atom) -> [Atom];
get_applications(List) when is_list(List) -> List.

get_input_modules(all) -> apxref_analyzer:all_modules();
get_input_modules(Modules) when is_list(Modules) -> Modules.

get_mode_type(undefined) ->
    export_and_import;
get_mode_type(Atom) when is_atom(Atom) ->
    Atom;
get_mode_type(List) when is_list(List) ->
    case {lists:member(export, List), lists:member(import, List)} of
        {true, true} -> export_and_import;
        {true, false} -> export;
        {false, true} -> import;
        _ -> erlang:error(wrong_mode)
    end.

get_module_calls(Modules, export) ->
    apxref_analyzer:module_calls(fun({F1, _F2}) -> lists:member(F1, Modules) end);
get_module_calls(Modules, import) ->
    apxref_analyzer:module_calls(fun({_F1, F2}) -> lists:member(F2, Modules) end);
get_module_calls(Modules, export_and_import) ->
    apxref_analyzer:module_calls(fun({F1, F2}) ->
        HaveIt1 = lists:member(F1, Modules),
        HaveIt2 = lists:member(F2, Modules),
        (HaveIt1 or HaveIt2)
    end).

get_dump_modules(false, Calls) ->
    AllModules0 = lists:foldl(fun(El, Res) ->
                                      {M1, M2} = El,
                                      [M1 | [M2 | Res]]
                              end, [], Calls),
    lists:usort(AllModules0);
get_dump_modules(true, _Calls) ->
    apxref_analyzer:all_modules().

get_app_modules(false, _AllModules) ->
    [];
get_app_modules(true, AllModules) ->
    AppMods0 = apxref_analyzer:application_modules(),
    [begin
         {App, Mods0} = El,
         Mods = lists:filter(fun(Mod) -> lists:member(Mod, AllModules) end, Mods0),
         {App, Mods}
     end || El <- AppMods0].
