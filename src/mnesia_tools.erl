-module(mnesia_tools).

%% Include files

%% Exported functions

-export([
    db_save/1,
    db_save/0,
    db_load/1,
    db_load/0
]).

%% API

db_save() ->
    db_save("mnesia_dump.bin").

db_save(FileName) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    Tables = mnesia:system_info(local_tables) -- [schema],
    Data =
    {[begin
         Attributes = mnesia:table_info(Table, attributes),
         Type = mnesia:table_info(Table, type),
         {Table, Type, Attributes}
     end || Table <- Tables],
     [begin
          FunGetAllRecords = fun() -> qlc:e(mnesia:table(Table)) end,
          {atomic, AllRecords} = mnesia:transaction(FunGetAllRecords),
          AllRecords
      end || Table <- Tables]},
    CompressedData = term_to_binary(Data, [compressed]),
    file:write_file(FileName, CompressedData).

db_load() ->
    db_load("mnesia_dump.bin").

db_load(FileName) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    {ok, CompressedData} = file:read_file(FileName),
    Data = binary_to_term(CompressedData),
    {Tables, TableRecords} = Data,
    [mnesia:create_table(Table, [{type, Type}, {disc_copies, [node()]}, {attributes, Attributes}]) || {Table, Type, Attributes} <- Tables],
    [[mnesia:dirty_write(Record) || Record <- Records] || Records <- TableRecords],
    ok.

%% Local functions
