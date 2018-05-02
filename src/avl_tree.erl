-module(avl_tree).

-export([
  new/0,
  insert/2,
  min/1,
  max/1
]).

-include("data_structures.hrl").

new() ->
  #avl_tree{
    value = undefined,
    left = undefined,
    right = undefined,
    height = 0
  }.

insert(Value, Tree) when Tree =:= undefined ->
  Leaf = new(),
  Leaf#avl_tree{value = Value};
insert(Value, Tree) when Tree#avl_tree.value =:= undefined ->
  Tree#avl_tree{value = Value};
insert(Value, Tree) when Tree#avl_tree.value > Value ->
  NewTree = Tree#avl_tree{left = insert(Value, Tree#avl_tree.left)},
  balance(NewTree);
insert(Value, Tree) when Tree#avl_tree.value =< Value ->
  NewTree = Tree#avl_tree{right = insert(Value, Tree#avl_tree.right)},
  balance(NewTree).

min(#avl_tree{value = Value, left = Left}) when
  Left =:= undefined ->
  Value;
min(#avl_tree{left = Left}) ->
  min(Left).

max(#avl_tree{value = Value, right = Right}) when
  Right =:= undefined ->
  Value;
max(#avl_tree{right = Right}) ->
  max(Right).

height(Tree) when is_record(Tree, avl_tree)->
  Tree#avl_tree.height;
height(undefined) ->
  0.

balance_factor(Tree) ->
  height(Tree#avl_tree.right) - height(Tree#avl_tree.left).


fix_height(Tree = #avl_tree{left = undefined, right = undefined}) ->
  Tree#avl_tree{height = 1};
fix_height(Tree = #avl_tree{left = Left, right = undefined}) ->
  Tree#avl_tree{height = Left#avl_tree.height + 1};
fix_height(Tree = #avl_tree{left = undefined, right = Right}) ->
  Tree#avl_tree{height = Right#avl_tree.height + 1};
fix_height(Tree = #avl_tree{left = Left, right = Right}) ->
  Tree#avl_tree{
    height = erlang:max(Left#avl_tree.height, Right#avl_tree.height) + 1
  }.

%% TODO reduce code
rotate_right(Tree) ->
  Q = Tree#avl_tree.left,
  NewTree = Tree#avl_tree{left = Q#avl_tree.right},
  fix_height(Q#avl_tree{right = fix_height(NewTree)}).

rotate_left(Tree) ->
  Q = Tree#avl_tree.right,
  NewTree = Tree#avl_tree{right = Q#avl_tree.left},
  fix_height(Q#avl_tree{left = fix_height(NewTree)}).

balance(Tree) ->
  FixedTree = fix_height(Tree),
  case balance_factor(FixedTree) of
    -2 ->
      case balance_factor(FixedTree#avl_tree.left) > 0 of
        true ->
          FixedTree#avl_tree{left = rotate_left(FixedTree#avl_tree.left)};
        false ->
          rotate_right(FixedTree)
      end;
    2 ->
      case balance_factor(FixedTree#avl_tree.right) < 0 of
        true ->
          FixedTree#avl_tree{right = rotate_right(FixedTree#avl_tree.right)};
        false ->
          rotate_left(FixedTree)
      end;
    _ ->
      FixedTree
  end.