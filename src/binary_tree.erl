-module(binary_tree).

-include("data_structures.hrl").

-define(EMPTY_LEAF, #binary_tree{
  size = 0,
  value = undefined,
  left = undefined,
  right = undefined
}).

-export([
  new/0,
  insert/2,
  delete/2,
  contains/2,
  min/1,
  max/1,
  size/1,
  minimums_test/1
]).

new() ->
  ?EMPTY_LEAF.

insert(Element, undefined) ->
  #binary_tree{
    size = 1,
    value = Element,
    left = ?EMPTY_LEAF,
    right = ?EMPTY_LEAF
  };
insert(Element, #binary_tree{size = 0, value = undefined}) ->
  #binary_tree{
    size = 1,
    value = Element,
    left = ?EMPTY_LEAF,
    right = ?EMPTY_LEAF
  };
insert(Element, Tree = #binary_tree{value = Value}) when Value =:= Element ->
  Tree;
insert(Element, Tree = #binary_tree{size = Size, value = Value, right = Right}) when Value < Element ->
  NewRight = insert(Element, Right),
  Tree#binary_tree{
    size = Size - Right#binary_tree.size + NewRight#binary_tree.size,
    right = NewRight
  };
insert(Element, Tree = #binary_tree{size = Size, value = Value, left = Left}) when Value > Element ->
  NewLeft = insert(Element, Left),
  Tree#binary_tree{
    size = Size - Left#binary_tree.size + NewLeft#binary_tree.size,
    left = NewLeft
  }.

delete(Element, #binary_tree{value = Value, left = ?EMPTY_LEAF, right = ?EMPTY_LEAF}) when Value =:= Element ->
  ?EMPTY_LEAF;
delete(Element, #binary_tree{value = Value, left = ?EMPTY_LEAF, right = Right}) when Value =:= Element ->
  Right;
delete(Element, #binary_tree{value = Value, left = Left, right = ?EMPTY_LEAF}) when Value =:= Element ->
  Left;
delete(Element, #binary_tree{size = Size, value = Value, left = Left, right = Right}) when Value =:= Element ->
  NewValue = min(Right),
  NewRight = delete(NewValue, Right),
  #binary_tree{
    size = Size - 1,
    value = NewValue,
    left = Left,
    right = NewRight
  };
delete(Element, Tree = #binary_tree{size = Size, value = Value, right = Right}) when Value < Element ->
  NewRight = delete(Element, Right),
  Tree#binary_tree{
    size = Size - Right#binary_tree.size + NewRight#binary_tree.size,
    right = NewRight
  };
delete(Element, Tree = #binary_tree{size = Size, value = Value, left = Left}) when Value > Element ->
  NewLeft = delete(Element, Left),
  Tree#binary_tree{
    size = Size - Left#binary_tree.size + NewLeft#binary_tree.size,
    left = NewLeft
  }.

contains(_Element, #binary_tree{size = 0, value = undefined}) ->
  false;
contains(Element, #binary_tree{value = Value}) when Value =:= Element ->
  true;
contains(Element, #binary_tree{value = Value, right = Right}) when Value > Element ->
  contains(Element, Right);
contains(Element, #binary_tree{value = Value, left = Left}) when Value < Element ->
  contains(Element, Left).

min(#binary_tree{value = Value, left = Left}) when
  Left =:= undefined;
  Left =:= ?EMPTY_LEAF ->
  Value;
min(#binary_tree{left = Left}) ->
  min(Left).

max(#binary_tree{value = Value, right = Right}) when
  Right =:= undefined;
  Right =:= ?EMPTY_LEAF ->
  Value;
max(#binary_tree{right = Right}) ->
  max(Right).

size(#binary_tree{size = Size}) ->
  Size.


minimums_test(_Config) ->
  List = [0,4,5,1,2,3],
  Tree = lists:foldl(
    fun(Elem, Acc) ->
      binary_tree:insert(Elem, Acc)
    end, binary_tree:new(), List),
  {SortedList, _} = lists:foldl(
    fun(_Elem, {Acc, AccTree}) ->
      Min = binary_tree:min(AccTree),
      NewTree = binary_tree:delete(Min, AccTree),
      {[Min|Acc], NewTree}
    end, {[], Tree}, lists:seq(1, binary_tree:size(Tree))),
  SortedList = lists:reverse(lists:sort(List)) .