-module(directed_graph).

-export([new/0, add_edge/2, to_list/1]).

-include("data_structures.hrl").

new() ->
  #directred_graph{container = ets:new(directed_graph, [ordered_set])}.

add_edge({From, To}, Graph = #directred_graph{container = Tid}) ->
  case ets:lookup(Tid, From) of
    [] ->
      ets:insert(Tid, {From, [To]});
    [{From, ToVertexes}] ->
      ets:insert(Tid, {From, [To|ToVertexes]})
  end,
  Graph.

to_list(#directred_graph{container = Tid}) ->
  lists:flatmap(
    fun({From, To}) ->
      lists:foldl(fun(Elem, Acc) -> [{From, Elem}|Acc] end, [], To)
    end, ets:tab2list(Tid)).