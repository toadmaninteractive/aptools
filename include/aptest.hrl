-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

proper_test_() ->
    aptest:run_props(?MODULE).
