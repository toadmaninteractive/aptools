-module(aptest).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([
    run_props/1
]).

%% API

run_props(Module) ->
    {timeout, 600,
    [{atom_to_list(F),
      fun () -> ?assert(proper:quickcheck(Module:F(), [long_result])) end}
      || {F, 0} <- Module:module_info(exports), F > 'prop_', F < 'prop`']}.

%% Local functions
