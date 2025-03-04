-module(apxref_dumper).

%% Include files

%% Exported functions

-export([
    dump_xrefs/4,
    run_graphviz/3
]).

%% API

dump_xrefs(FileName, AppModules, AllModules, Calls) ->
    {ok, FileDescriptor} = file:open(FileName, [write]),
    file:write(FileDescriptor, "digraph name {\n"),

    dump_clusters(FileDescriptor, AppModules),

    [begin
        Bin =  atom_to_binary(El, latin1),
        file:write(FileDescriptor, <<"   ", Bin/binary, ";\n">>)
     end || El <- AllModules],

    file:write(FileDescriptor, <<"\n">>),

    [begin
        {F1, F2} = El,
        BF1 =  atom_to_binary(F1, latin1),
        BF2 =  atom_to_binary(F2, latin1),
        file:write(FileDescriptor, <<"   ", BF1/binary, " -> ", BF2/binary, ";\n">>)
     end || El <- Calls],

    file:write(FileDescriptor, "}\n"),
    file:close(FileDescriptor),
    ok.

run_graphviz(GraphvizPath, InFile, OutFile) ->
    Cmd = binary_to_list(list_to_binary(io_lib:format("\"~s\" -Tpng -o ~s ~s", [GraphvizPath, OutFile, InFile]))),
    os:cmd(Cmd),
    os:cmd(OutFile),
    % open_port({spawn_executable, GraphvizPath}, [{args, ["-Tpng", "-o", OutFile, InFile]}]),
    ok.

%% Local functions

dump_clusters(FileDescriptor, AppModules) ->
    [begin
        {App, Modules} = El,
        dump_cluster(FileDescriptor, App, Modules)
    end || El <- AppModules].

dump_cluster(_FileDescriptor, _App, []) -> ok;
dump_cluster(FileDescriptor, App, Modules) ->
    BApp = atom_to_binary(App, latin1),
    file:write(FileDescriptor, <<"subgraph cluster_", BApp/binary, "\n{\n">>),
    file:write(FileDescriptor, <<"    node [style=filled,color=white];\n">>),
    file:write(FileDescriptor, <<"    style=filled;\n">>),
    file:write(FileDescriptor, <<"    color=lightgrey;\n">>),
    file:write(FileDescriptor, <<"    label = \"", BApp/binary, "\";\n">>),
    file:write(FileDescriptor, <<"    fontcolor = blue;\n">>),
    file:write(FileDescriptor, <<"    fontsize = 24;\n">>),

    [begin
        BMod = atom_to_binary(Mod, latin1),
        file:write(FileDescriptor, <<"    ", BMod/binary, ";\n">>)
    end || Mod <- Modules],

    file:write(FileDescriptor, <<"}\n\n">>).
