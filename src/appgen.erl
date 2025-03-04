-module(appgen).

%% Include files

%% Exported functions

-export([
    gen/1
]).

%% API

gen(Apps) when is_list(Apps) ->
    [ gen(App) || App <- Apps ],
    ok;
gen(App) ->
    io:format("gen ~p~n", [App]),
    Dir = filename:join(code:lib_dir(App), "ebin"),
    AppSrc = io_lib:format("~w.app.src", [App]),
    AppSrcFile = filename:join([code:lib_dir(App), "src", AppSrc]),
    {ok, Files} = file:list_dir(Dir),
    BeamFiles = lists:reverse([list_to_atom(filename:rootname(File)) || File <- Files, filename:extension(File) =:= ".beam"]),
    AppFileName = filename:join(Dir, atom_to_list(App) ++ ".app"),
    {ok, [{application, App, OldProps}]} = file:consult(AppSrcFile),
    NewProps = lists:keystore(modules, 1, OldProps, {modules, BeamFiles}),
    NewAppFile = io_lib:fwrite("~p.~n",[{application, App, NewProps}]),
    file:write_file(AppFileName, NewAppFile).

%% Local functions
