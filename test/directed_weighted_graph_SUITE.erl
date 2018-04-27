-module(directed_weighted_graph_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("data_structures.hrl").

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
  Graph = directed_weighted_graph:add_edge({{1, 2}, 1}, directed_weighted_graph:new()),
  ?assertEqual([#weighted_edge{route = {1, 2}, weight = 1}], directed_weighted_graph:to_list(Graph)).

delete_edge_test(_Config) ->
  Graph = directed_weighted_graph:add_edge({{1, 2}, 1}, directed_weighted_graph:new()),
  directed_weighted_graph:delete_edge({1, 2}, Graph),
  ?assertEqual([], directed_weighted_graph:to_list(Graph)).

delete_vertex_test(_Config) ->
  ToDeleteVertex = 1,
  List = [{{1, 2}, 1}, {{2, 1}, 1}, {{2, 3}, 1}, {{3, 2}, 1}, {{1, 3}, 1}],
  FilteredList = lists:filter(
    fun({{From, To}, _Weight}) ->
      From /= ToDeleteVertex andalso To /= ToDeleteVertex
    end, List),
  WrappedList = lists:map(
    fun({Route = {_From, _To}, Weight}) ->
      #weighted_edge{route = Route, weight = Weight}
    end, FilteredList),
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_weighted_graph:add_edge(Elem, Acc)
    end, directed_weighted_graph:new(), List),
  Graph = directed_weighted_graph:delete_vertex(ToDeleteVertex, Graph),
  ?assertEqual(lists:sort(WrappedList), directed_weighted_graph:to_list(Graph)).

delete_not_existing_vertex(_Config) ->
  ToDeleteVertex = 4,
  List = [{{1, 2}, 1}, {{2, 1}, 1}, {{2, 3}, 1}, {{3, 2}, 1}, {{1, 3}, 1}],
  FilteredList = lists:filter(
    fun({{From, To}, _Weight}) ->
      From /= ToDeleteVertex andalso To /= ToDeleteVertex
    end, List),
  WrappedList = lists:map(
    fun({Route = {_From, _To}, Weight}) ->
      #weighted_edge{route = Route, weight = Weight}
    end, FilteredList),
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_weighted_graph:add_edge(Elem, Acc)
    end, directed_weighted_graph:new(), List),
  Graph = directed_weighted_graph:delete_vertex(ToDeleteVertex, Graph),
  ?assertEqual(lists:sort(WrappedList), directed_weighted_graph:to_list(Graph)).

multiple_add_edge_test(_Config) ->
  List = [{{1, 2}, 1}, {{2, 1}, 1}, {{2, 3}, 1}, {{3, 2}, 1}, {{1, 3}, 1}],
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_weighted_graph:add_edge(Elem, Acc)
    end, directed_weighted_graph:new(), List),
  WrappedList = lists:map(
    fun({Route = {_From, _To}, Weight}) ->
      #weighted_edge{route = Route, weight = Weight}
    end, List),
  ?assertEqual(lists:sort(WrappedList), directed_weighted_graph:to_list(Graph)).

multiple_delete_edge_test(_Config) ->
  List = [{{1, 2}, 1}, {{2, 1}, 1}, {{2, 3}, 1}, {{3, 2}, 1}, {{1, 3}, 1}],
  Graph = lists:foldl(
    fun(Elem, Acc) ->
      directed_weighted_graph:add_edge(Elem, Acc)
    end, directed_weighted_graph:new(), List),
  WrappedList = lists:map(
    fun({Route = {_From, _To}, Weight}) ->
      #weighted_edge{route = Route, weight = Weight}
    end, List),
  ?assertEqual(lists:sort(WrappedList), directed_weighted_graph:to_list(Graph)),
  lists:foreach(
    fun({Route = {_From, _To}, _Weight}) ->
      directed_weighted_graph:delete_edge(Route, Graph)
    end, List),
  ?assertEqual([], directed_weighted_graph:to_list(Graph)).

multiple_graphs_test(_Config) ->
  Graph0 = directed_weighted_graph:add_edge({{1, 2}, 1}, directed_weighted_graph:new()),
  Graph1 = directed_weighted_graph:add_edge({{3, 4}, 1}, directed_weighted_graph:new()),
  ?assertEqual([#weighted_edge{route = {1, 2}, weight = 1}], directed_weighted_graph:to_list(Graph0)),
  ?assertEqual([#weighted_edge{route = {3, 4}, weight = 1}], directed_weighted_graph:to_list(Graph1)).

from_list_test(_Config) ->
  List = [{{1, 2}, 1}, {{2, 1}, 1}, {{2, 3}, 1}, {{3, 2}, 1}, {{1, 3}, 1}],
  WrappedList = lists:map(
    fun({Route = {_From, _To}, Weight}) ->
      #weighted_edge{route = Route, weight = Weight}
    end, List),
  Graph = directed_weighted_graph:from_list(List),
  ?assertEqual(lists:sort(WrappedList), directed_weighted_graph:to_list(Graph)).

delete_test(_Config) ->
  List = [{{1, 2}, 1}, {{2, 1}, 1}, {{2, 3}, 1}, {{3, 2}, 1}, {{1, 3}, 1}],
  WrappedList = lists:map(
    fun({Route = {_From, _To}, Weight}) ->
      #weighted_edge{route = Route, weight = Weight}
    end, List),
  Graph = directed_weighted_graph:from_list(List),
  ?assertEqual(lists:sort(WrappedList), directed_weighted_graph:to_list(Graph)),
  directed_weighted_graph:delete(Graph),
  ?assertError(badarg, directed_weighted_graph:to_list(Graph)).