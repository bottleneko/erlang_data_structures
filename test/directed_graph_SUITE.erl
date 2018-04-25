-module(directed_graph_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    add_test,
    multiple_add_test,
    multiple_graphs_test
  ].

add_test(_Config) ->
  Graph = directed_graph:add_edge({1, 2}, directed_graph:new()),
  ?assertEqual([{1, 2}], directed_graph:to_list(Graph)).


multiple_add_test(_Config) ->
  List = [{1, 2}, {2, 1}, {2, 3}, {3, 2}, {1, 3}],
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_graph:add_edge(Elem, Acc)
    end, directed_graph:new(), List),
?assertEqual(lists:sort(List), directed_graph:to_list(Graph)).

multiple_graphs_test(_Config) ->
  Graph0 = directed_graph:add_edge({1, 2}, directed_graph:new()),
  Graph1 = directed_graph:add_edge({3, 4}, directed_graph:new()),
  ?assertEqual([{1, 2}], directed_graph:to_list(Graph0)),
  ?assertEqual([{3, 4}], directed_graph:to_list(Graph1)).