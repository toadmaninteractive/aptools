-module(approf).

%% Include files

%% Exported functions

-export([
    cprof/1,
    cprof/2,
    fprof/1,
    fprof/3,
    profile/0,
    profile/1
]).

%% API

cprof(Time) ->
    cprof(Time, "cprof.txt").

cprof(Time, FileName) ->
    spawn(
        fun() ->
            cprof:stop(),
            cprof:start(),
            timer:sleep(Time * 1000),
            cprof:pause(),
            Result = cprof:analyse(),
            File = io_lib:fwrite("~p~n",[Result]),
            file:write_file(FileName, File),
            cprof:stop()
        end).

-spec fprof(Seconds :: number()) -> any().

fprof(Seconds) ->
    fprof(Seconds, all, "fprof.trace").

-spec fprof(Seconds, PidSpecOrSpecs, FileName) -> any() when
      Seconds :: number(),
      PidSpecOrSpecs :: PidSpec | [PidSpec],
      PidSpec :: 'new' | 'existing' | 'new' | pid(),
      FileName :: string().

fprof(Seconds, PidSpecOrSpecs, FileName) ->
    fprof:trace([start, {file, FileName}, {procs, PidSpecOrSpecs}]),
    timer:sleep(round(Seconds * 1000)),
    erlang:trace(all, false, [all]),
    fprof:trace(stop).

profile() ->
    profile([]).

profile(Options) ->
    fprof:profile({file, "fprof.trace"}),
    fprof:analyse([{dest, "fprof.analysis"}, {cols, 120} | Options]).

%% Local functions
