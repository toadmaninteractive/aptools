-module(mnesia_backup_tools).

%% Include files

%% Exported functions

-export([
    backup/1,
    restore/1,
    switch_node/2,
    switch_node/3
]).

-export([
    open_write/1,
    write/2,
    commit_write/1,
    abort_write/1,

    open_read/1,
    read/1,
    close_read/1
]).

%% API

backup(Path) ->
    mnesia:backup(Path, ?MODULE).

restore(Path) ->
    mnesia:restore(Path, [{module, ?MODULE}, {default_op, recreate_tables}]).

switch_node(Source, Target) ->
    Switch = fun(_Node) -> node() end,
    switch_node(Switch, Source, Target).

switch_node(Switch, Source, Target) ->
    Convert =
        fun({schema, db_nodes, Nodes}, Acc) ->
                {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
           ({schema, version, Version}, Acc) ->
                {[{schema, version, Version}], Acc};
           ({schema, cookie, Cookie}, Acc) ->
                {[{schema, cookie, Cookie}], Acc};
           ({schema, Tab, CreateList}, Acc) ->
                Keys = [ram_copies, disc_copies, disc_only_copies],
                OptSwitch =
                    fun({Key, Val}) ->
                            case lists:member(Key, Keys) of
                                true -> {Key, lists:map(Switch, Val)};
                                false-> {Key, Val}
                            end
                    end,
                {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc};
           (Other, Acc) ->
                {[Other], Acc}
        end,
    mnesia:traverse_backup(Source, ?MODULE, Target, ?MODULE, Convert, switched).

%% Backup interface

open_write(FileName) ->
    file:open(FileName, [write, binary]).

write(File, BackupItems) ->
    PackedData = term_to_binary(BackupItems, [{compressed, 3}, {minor_version, 1}]),
    DataSize = size(PackedData),
    case file:write(File, <<DataSize:32>>) of
        ok ->
            file:write(File, PackedData),
            {ok, File};
        {error, Reason} -> {error, Reason}
    end.

commit_write(File) ->
    file:close(File),
    {ok, File}.

abort_write(BackupRef) ->
    {ok, BackupRef}.

%% Restore interface

open_read(FileName) ->
    file:open(FileName, [read, binary]).

read(File) ->
    case file:read(File, 4) of
        {ok, <<ChunkSize:32>>} ->
            {ok, Data} = file:read(File, ChunkSize),
            {ok, File, binary_to_term(Data)};
        _ ->
            {ok, File, []}
    end.

close_read(File) ->
    file:close(File),
    {ok, File}.

%% Local functions
