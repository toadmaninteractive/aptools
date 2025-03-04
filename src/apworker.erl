-module(apworker).

%% Include files

%% Exported functions

-export([
    work/4,
    work/3,
    spawn_work/4,
    stop/1,
    track/2,

    test/0
]).

-record(worker, {
    tasks,
    total,
    wait,
    callback,
    thread_count,
    thread_max_count,
    acc
}).

-type callback() :: fun((event()) -> result()).
-type event() :: {'task', task()} | {'track', integer(), integer()} | {'data', task(), result(), term()} | {'stop', term(), [task()]}.
-type task() :: term().
-type result() :: term() | 'undefined'.

%% API

spawn_work(Tasks, ThreadCount, Fun, Acc) ->
    erlang:spawn(fun() ->
        work(Tasks, ThreadCount, Fun, Acc)
    end).

stop(Pid) ->
    Self = self(),
    Pid ! {stop, Self},
    receive
        Data -> Data
    after 5000 ->
        undefined
    end.

work(Tasks, ThreadCount, Fun) ->
    work(Tasks, ThreadCount, simple(Fun), undefined).

-spec work(Tasks :: [task()], Count :: integer(), Fun :: callback(), Acc :: term()) -> result().

work(Tasks, ThreadCount, Fun, Acc) when is_list(Tasks), is_function(Fun, 1), ThreadCount > 0 ->
    State = #worker{
        tasks = Tasks,
        total = length(Tasks),
        wait = [],
        callback = Fun,
        thread_count = 0,
        thread_max_count = ThreadCount,
        acc = Acc
    },
    loop(State).

simple(Fun) ->
    fun
        ({task, Task}) ->
            Fun(Task);
        ({track, Current, Total}) ->
            track(Current, Total);
        (_) ->
            undefined
    end.

track(Current, Total) ->
    P = round(Current/Total*100),
    if
        P =/= round((Current+1)/Total*100) -> io:format("Done: ~p%~n", [100 - P]);
        true -> ignore
    end.

%% Internal tests

test() ->
    work(lists:seq(1, 100), 10, fun test_callback/1, 0).

test_callback({task, Task}) ->
    timer:sleep(100),
    Task;
test_callback({track, Current, Total}) ->
    track(Current, Total);
test_callback({data, _Task, Data, Acc}) ->
    Data + Acc;
test_callback({stop, Acc, _Tasks}) ->
    Acc.

%% Local functions

spawn_worker(Task, Fun) ->
    Self = self(),
    erlang:spawn_link(fun() ->
        try
            Result = Fun({task, Task}),
            Self ! {data, Task, Result}
        catch
            error:Reson ->
                io:format("error(~p): ~p~n ~p~n", [Task, Reson, erlang:get_stacktrace()]),
                Self ! {data, Task, undefined}
        end
    end).

loop(#worker{tasks = [], thread_count = 0} = State) ->
    #worker{acc = Acc, callback = Fun} = State,
    Fun({stop, Acc, []});
loop(#worker{tasks = [Task|Tasks], thread_count = Count, thread_max_count = MaxCount} = State) when Count < MaxCount ->
    #worker{
        total = Total,
        wait = Wait,
        callback = Fun
    } = State,
    Current = length(Tasks),
    Fun({track, Current, Total}),
    spawn_worker(Task, Fun),
    NewState = State#worker{
        tasks = Tasks,
        wait = [Task|Wait],
        thread_count = Count + 1
    },
    loop(NewState);
loop(State) ->
    receive
        {data, Task, Data} ->
            #worker{
                acc = Acc,
                wait = Wait,
                thread_count = Count,
                callback = Fun
            } = State,
            NewWait = Wait -- [Task],
            NewAcc = Fun({data, Task, Data, Acc}),
            NewState = State#worker{
                acc = NewAcc,
                wait = NewWait,
                thread_count = Count - 1
            },
            loop(NewState);
        {stop, From} ->
            #worker{
                acc = Acc,
                tasks = Tasks,
                wait = Wait,
                callback = Fun
            } = State,
            NotDone = Wait ++ Tasks,
            Result = Fun({stop, Acc, NotDone}),
            From ! Result;
        Msg ->
            io:format("unkown: ~p~n", [Msg]),
            loop(State)
    end.
