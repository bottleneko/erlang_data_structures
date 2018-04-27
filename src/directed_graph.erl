-module(directed_graph).

-export([
  new/0,
  add_edge/2,
  delete_edge/2,
  delete_vertex/2,
  to_list/1,
  from_list/1,
  delete/1
]).

-include("data_structures.hrl").

new() ->
  #directed_graph{container = ets:new(directed_graph, [ordered_set])}.

add_edge({From, To}, Graph = #directed_graph{container = Tid}) ->
  case ets:lookup(Tid, From) of
    [] ->
      ets:insert(Tid, {From, [To]});
    [{From, ToVertexes}] ->
      ets:insert(Tid, {From, [To|ToVertexes]})
  end,
  Graph.

delete_edge({From, To}, Graph = #directed_graph{container = Tid}) ->
  case ets:lookup(Tid, From) of
    [] ->
      ok;
    [{From, ToVertexes}] ->
      case lists:delete(To, ToVertexes) of
        [] ->
          ets:delete(Tid, From);
        List ->
          ets:insert(Tid, {From, List})
      end
  end,
  Graph.

delete_vertex(Vertex, Graph = #directed_graph{container = Tid}) ->
  ets:delete(Tid, Vertex),
  delete_vertex_T(Vertex, Tid, ets:first(Tid)),
  Graph.

delete_vertex_T(_Vertex, _Tid, Iterator) when Iterator =:= '$end_of_table' ->
  ok;
delete_vertex_T(Vertex, Tid, Key) ->
  [{From, To}] = ets:lookup(Tid, Key),
  case lists:delete(Vertex, To) of
    To ->
      ok;
    [] ->
      ets:delete(Tid, From);
    NewTo ->
      ets:insert(Tid, {From, NewTo})
  end,
  delete_vertex_T(Vertex, Tid, ets:next(Tid, Key)).


to_list(#directed_graph{container = Tid}) ->
  lists:flatmap(
    fun({From, To}) ->
      lists:foldl(fun(Elem, Acc) -> [{From, Elem}|Acc] end, [], To)
    end, ets:tab2list(Tid)).

from_list(List) ->
  Graph = lists:foldl(
    fun({From, To}, Acc) ->
      Graph = Acc#directed_graph.container,
      case ets:lookup(Graph, From) of
        [] ->
          ets:insert(Graph, {From, [To]});
        [{From, ToVertexes}] ->
          ets:insert(Graph, {From, [To|ToVertexes]})
      end,
      Acc
    end, new(), List),
  Graph.

delete(#directed_graph{container = Tid}) ->
  ets:delete(Tid).