-module(snmp_handler_erlang).

%% Include files

%% Exported functions

-export([
    snmp_name/0,
    snmp_id/0,

    handle_memoryProcesses/1,
    handle_memoryProcessesUsed/1,
    handle_memorySystem/1,
    handle_memoryAtom/1,
    handle_memoryAtomUsed/1,
    handle_memoryBinary/1,
    handle_memoryCode/1,
    handle_memoryEts/1,
    handle_memoryTotal/1,
    handle_portCount/1,
    handle_processCount/1
]).

%% API

snmp_name() ->
    "erlang".

snmp_id() ->
    1.

handle_memoryProcesses(get) ->
    {value, erlang:memory(processes)}.

handle_memoryProcessesUsed(get) ->
    {value, erlang:memory(processes_used)}.

handle_memorySystem(get) ->
    {value, erlang:memory(system)}.

handle_memoryAtom(get) ->
    {value, erlang:memory(atom)}.

handle_memoryAtomUsed(get) ->
    {value, erlang:memory(atom_used)}.

handle_memoryBinary(get) ->
    {value, erlang:memory(binary)}.

handle_memoryCode(get) ->
    {value, erlang:memory(code)}.

handle_memoryEts(get) ->
    {value, erlang:memory(ets)}.

handle_memoryTotal(get) ->
    {value, erlang:memory(total)}.

handle_portCount(get) ->
    {value, erlang:system_info(port_count)}.

handle_processCount(get) ->
    {value, erlang:system_info(process_count)}.

%% Local functions
