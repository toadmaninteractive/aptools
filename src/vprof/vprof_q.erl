-module(vprof_q).

%% Include files

%% Exported functions

-export([
    group/2,
    q/2,
    qq/2
]).

%% API

group(K, List) ->
    lists:foldl(
        fun(Item, Acc) ->
            Key = q(K, Item),
            orddict:update(Key, fun(L) -> [Item|L] end, [Item], Acc)
        end, orddict:new(), List).

q(N, V) when is_integer(N) ->
    element(N, V);
q(id, V) ->
    V;
q(max, L) ->
    lists:max(L);
q(min, L) ->
    lists:min(L);
q(sum, L) ->
    lists:sum(L);
q(length, L) ->
    length(L);
q('++', L) ->
    lists:append(L);
q(ordset, L) ->
    ordsets:from_list(L);
q({group, K}, L) ->
    group(K, L);
q([Q], V) ->
    [ q(Q, I) || I <- V];
q(T, Item) when is_tuple(T) ->
    list_to_tuple([q(Q,Item) || Q <- tuple_to_list(T)]);
q(F, Item) when is_function(F, 1) ->
    F(Item).

qq(Source, [Q|Qs]) ->
    qq(q(Q, Source), Qs);
qq(Source, []) ->
    Source.

%% Local functions
