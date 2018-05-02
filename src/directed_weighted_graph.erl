-module(directed_weighted_graph).

-export([
  new/0,
  add_edge/2,
  delete_edge/2,
  delete_vertex/2,
  to_list/1,
  from_list/1,
  delete/1,
  incident_edges/2
]).

-include_lib("stdlib/include/ms_transform.hrl").

-include("data_structures.hrl").

new() ->
  #directed_weighted_graph{container = ets:new(directed_weighted_graph, [ordered_set, {keypos, 2}])}.

add_edge(WeightedEdge, Graph = #directed_weighted_graph{container = Tid}) when
  is_record(WeightedEdge, weighted_edge) ->
  ets:insert(Tid, WeightedEdge),
  Graph;
add_edge({Route = {_From, _To}, Weight}, Graph = #directed_weighted_graph{container = Tid}) ->
  ets:insert(Tid, #weighted_edge{route = Route, weight = Weight}),
  Graph.

delete_edge(Route = {_From, _To}, Graph = #directed_weighted_graph{container = Tid}) ->
  ets:delete(Tid, Route),
  Graph.

delete_vertex(Vertex, Graph = #directed_weighted_graph{container = Tid}) ->
  ets:delete(Tid, Vertex),
  ets:select_delete(Tid, ets:fun2ms(
    fun(#weighted_edge{route = {From, To}}) when
      From =:= Vertex; To =:= Vertex ->
      true
    end)),
  Graph.

to_list(#directed_weighted_graph{container = Tid}) ->
  ets:tab2list(Tid).

from_list(List) ->
  Graph = lists:foldl(
    fun(Edge, Acc) ->
      add_edge(Edge, Acc)
    end, new(), List),
  Graph.

delete(#directed_weighted_graph{container = Tid}) ->
  ets:delete(Tid).

incident_edges(Vertex, #directed_weighted_graph{container = Tid}) ->
  ets:select(Tid, ets:fun2ms(
    fun(Edge = #weighted_edge{route = {From, To}}) when
      From =:= Vertex;
      To =:= Vertex ->
      Edge
    end)).