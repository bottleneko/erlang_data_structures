-module(directed_graph_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    add_edge_test,
    delete_edge_test,
    delete_vertex_test,
    delete_not_existing_vertex,
    multiple_add_edge_test,
    multiple_delete_edge_test,
    multiple_graphs_test,
    from_list_test,
    delete_test
  ].

add_edge_test(_Config) ->
  Graph = directed_graph:add_edge({1, 2}, directed_graph:new()),
  ?assertEqual([{1, 2}], directed_graph:to_list(Graph)).

delete_edge_test(_Config) ->
  Graph = directed_graph:add_edge({1, 2}, directed_graph:new()),
  directed_graph:delete_edge({1, 2}, Graph),
  ?assertEqual([], directed_graph:to_list(Graph)).

delete_vertex_test(_Config) ->
  ToDeleteVertex = 1,
  List = [{1, 2}, {2, 1}, {2, 3}, {3, 2}, {1, 3}],
  FilteredList = lists:filter(
    fun({From, To}) ->
      From /= ToDeleteVertex andalso To /= ToDeleteVertex
    end, List),
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_graph:add_edge(Elem, Acc)
    end, directed_graph:new(), List),
  Graph = directed_graph:delete_vertex(ToDeleteVertex, Graph),
  ?assertEqual(lists:sort(FilteredList), directed_graph:to_list(Graph)).

delete_not_existing_vertex(_Config) ->
  ToDeleteVertex = 4,
  List = [{1, 2}, {2, 1}, {2, 3}, {3, 2}, {1, 3}],
  FilteredList = lists:filter(
    fun({From, To}) ->
      From /= ToDeleteVertex andalso To /= ToDeleteVertex
    end, List),
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_graph:add_edge(Elem, Acc)
    end, directed_graph:new(), List),
  Graph = directed_graph:delete_vertex(ToDeleteVertex, Graph),
  ?assertEqual(lists:sort(FilteredList), directed_graph:to_list(Graph)).

multiple_add_edge_test(_Config) ->
  List = [{1, 2}, {2, 1}, {2, 3}, {3, 2}, {1, 3}],
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_graph:add_edge(Elem, Acc)
    end, directed_graph:new(), List),
  ?assertEqual(lists:sort(List), directed_graph:to_list(Graph)).

multiple_delete_edge_test(_Config) ->
  List = [{1, 2}, {2, 1}, {2, 3}, {3, 2}, {1, 3}],
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_graph:add_edge(Elem, Acc)
    end, directed_graph:new(), List),
  ?assertEqual(lists:sort(List), directed_graph:to_list(Graph)),
  lists:foreach(
    fun(Elem) ->
      directed_graph:delete_edge(Elem, Graph)
    end, List),
  ?assertEqual([], directed_graph:to_list(Graph)).

multiple_graphs_test(_Config) ->
  Graph0 = directed_graph:add_edge({1, 2}, directed_graph:new()),
  Graph1 = directed_graph:add_edge({3, 4}, directed_graph:new()),
  ?assertEqual([{1, 2}], directed_graph:to_list(Graph0)),
  ?assertEqual([{3, 4}], directed_graph:to_list(Graph1)).

from_list_test(_Config) ->
  List = [{1, 2}, {2, 1}, {2, 3}, {3, 2}, {1, 3}],
  Graph = directed_graph:from_list(List),
  ?assertEqual(lists:sort(List), directed_graph:to_list(Graph)).

delete_test(_Config) ->
  List = [{1, 2}, {2, 1}, {2, 3}, {3, 2}, {1, 3}],
  Graph = directed_graph:from_list(List),
  ?assertEqual(lists:sort(List), directed_graph:to_list(Graph)),
  directed_graph:delete(Graph),
  ?assertError(badarg, directed_graph:to_list(Graph)).