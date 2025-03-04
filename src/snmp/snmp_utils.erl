-module(snmp_utils).

%% Include files

%% Exported functions

-export([
    v3keys/3,
    gen_mibs/3,
    gen_heads/0,

    compile/3
]).

%{snmp, [
%    {agent, [
%        {versions, [v2]},
%        {db_init_error, create_db_and_dir},
%        {db_dir, "var/snmp"},
%        {mibs, [
%            "etc/mibs/erlang.bin"
%        ]},
%        {config, [
%            {force_load, true},
%            {dir, "etc/agent"}
%        ]}
%    ]}
%]}

%% LINUX SYNTAX
% snmpwalk -v2c -c [Community] 127.0.0.1:25000 .1
% snmpwalk -v3 -u [User] -a md5 -A [AuthKey] -X [PrivKey] -l authPriv 127.0.0.1:25000 .1

%% WINDOWS SYNTAX
% snmpwalk -v:2c -c:[Comunity] -r:127.0.0.1 -p:25000

%% API

v3keys(AuthKey, PrivKey, Engine) ->
    application:start(crypto),
    {
        snmp:passwd2localized_key(md5, AuthKey, Engine),
        snmp:passwd2localized_key(md5, PrivKey, Engine)
    }.

gen_mibs(ProjName, ProjId, Modules) ->
    lists:foreach(fun(M) ->
        compile(ProjName, ProjId, M)
    end, Modules).

gen_heads() ->
    {ok, Files} = file:list_dir("etc/mibs"),
    [snmpc:mib_to_hrl("etc/mibs/" ++ filename:rootname(File)) || File <- Files].

compile(ProjName, ProjId, Module) ->
    Name = Module:snmp_name(),
    gen(ProjName, ProjId, Module, Name, Module:snmp_id(), props(Module)),
    snmpc:compile(Name, [{outdir, "etc/mibs"}, {db, volatile}]),
    file:delete(Name ++ ".funcs"),
    file:delete(Name ++ ".mib"),
    ok.

%% Local functions

props(Module) ->
    List = [prop_name(atom_to_list(A)) || {A, 1} <- Module:module_info(exports)],
    [A || A <- List, A =/= undefined].

gen(ProjName, ProjId, Module, Name, Id, Props) ->
    file:write_file(Name ++ ".funcs", gen_funcs(Module, Props)),
    file:write_file(Name ++ ".mib", gen_mib(ProjName, ProjId, Name, Id, Props)),
    ok.

prop_name([$h, $a, $n, $d, $l, $e, $_ | Tail]) ->
    Tail;
prop_name(_) ->
    undefined.

gen_funcs(Module, Props) ->
    [
        io_lib:format("{ ~s, { ~s, handle_~s, []}}.\n", [Name, Module, Name])
    || Name <- Props].

gen_mib(ProjName, ProjId, Name, Id, Props) ->
    ObjId = Name ++ "Id",
    List = lists:zip(Props, lists:seq(1, length(Props))),
    [
        io_lib:format(
            "~s DEFINITIONS ::= BEGIN\n\n"
            "IMPORTS\n"
            "    enterprises, IpAddress, Counter\n"
            "        FROM RFC1155-SMI\n"
            "    DisplayString\n"
            "        FROM RFC1213-MIB\n"
            "    OBJECT-TYPE\n"
            "        FROM RFC-1212;\n\n"
            "~s OBJECT IDENTIFIER ::= { enterprises ~p }\n"
            "~s OBJECT IDENTIFIER ::= { ~s ~p }\n", [
            string:to_upper(Name), ProjName, ProjId, ObjId, ProjName, Id
        ]),
        [gen_mib_int(ObjId, N, Index) || {N, Index} <- List],
        "\nEND"
    ].

gen_mib_int(ObjId, Name, Index) ->
    io_lib:format(
        "\n~s OBJECT-TYPE\n"
        "    SYNTAX INTEGER\n"
        "    ACCESS  read-write\n"
        "    STATUS  mandatory\n"
        "    DESCRIPTION \"~s\"\n"
        "    ::= { ~s ~p }\n", [
        Name, Name, ObjId, Index
    ]).
