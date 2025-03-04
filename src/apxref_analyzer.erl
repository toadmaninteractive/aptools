-module(apxref_analyzer).

%% Include files

%% Exported functions

-export([
    start/1,
    stop/0
]).

-export([
    all_modules/0,
    application_modules/0,
    module_calls/1,
    module_calls/0
]).

-define(xref_name, xref_name).

%% API

start(Applications) ->
    Paths0 = [begin
                Res = code:lib_dir(App),
                case Res of
                    {error, _} ->
                        io:format("Application ~w is not found or has no modules.~n", [App]),
                        false;
                    _ ->
                        Res
                end,
                Res
             end || App <- Applications],

    Paths = lists:filter(fun(El) -> El =/= false end, Paths0),

    xref:start(?xref_name),
    xref:set_default(?xref_name, [{verbose,false}, {warnings,false}]),
    [xref:add_application(?xref_name, El) || El <- Paths],
    %% xref:analyse(s, undefined_function_calls),
    ok.

stop() ->
     xref:stop(?xref_name),
     ok.

all_modules() ->
    {ok, Modules} = xref:q(?xref_name, "AM"),
    lists:filter(fun(El) -> string:str(atom_to_list(El), "_test") == 0 end, Modules).

application_modules() ->
     {ok, Apps} = xref:q(?xref_name, "A"),
     [begin
        Queue = string:concat("(Mod) ", atom_to_list(El)),
        {ok, Modules} = xref:q(?xref_name, Queue),
        {El, Modules}
     end || El <- Apps].

module_calls(Filter) ->
    Calls = module_calls(),
    lists:filter(Filter, Calls).

module_calls() ->
    AllModules = all_modules(),
    {ok, ModuleCalls0} = xref:q(?xref_name, "strict ME"),
    ModuleCalls1 = lists:usort(ModuleCalls0),
    lists:filter(fun({F1, F2}) ->
        HaveIt1 = lists:member(F1, AllModules),
        HaveIt2 = lists:member(F2, AllModules),
        (HaveIt1 and HaveIt2)
    end, ModuleCalls1).

%% Local functions
