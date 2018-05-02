-module(avl_tree).

-export([
  new/0,
  insert/2,
  min/1,
  max/1,
  size/1,
  delete/2
]).

-include("data_structures.hrl").

new() ->
  #avl_tree{
    value = undefined,
    size = 0,
    height = 0,
    left = undefined,
    right = undefined
  }.

insert(Value, undefined) ->
  Leaf = new(),
  Leaf#avl_tree{value = Value, size = 1, height = 1};
insert(Value, Tree) when Tree#avl_tree.value =:= undefined ->
  Tree#avl_tree{value = Value, size = 1, height = 1};
insert(Value, Tree = #avl_tree{size = Size, left = Left}) when Tree#avl_tree.value > Value ->
  NewLeft = insert(Value, Left),
  NewTree = Tree#avl_tree{
    size = Size - safe_size(Left) + NewLeft#avl_tree.size,
    left = NewLeft
  },
  balance(NewTree);
insert(Value, Tree = #avl_tree{size = Size, right = Right}) when Tree#avl_tree.value =< Value ->
  NewRight = insert(Value, Right),
  NewTree = Tree#avl_tree{
    size = Size - safe_size(Right) + NewRight#avl_tree.size,
    right = NewRight
  },
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

size(Tree) ->
  safe_size(Tree).

delete(Element, #avl_tree{value = Value, left = undefined, right = undefined}) when Value =:= Element ->
  undefined;
delete(Element, #avl_tree{value = Value, left = undefined, right = Right}) when Value =:= Element ->
  Right;
delete(Element, #avl_tree{value = Value, left = Left, right = undefined}) when Value =:= Element ->
  Left;
delete(Element, #avl_tree{size = Size, value = Value, left = Left, right = Right}) when Value =:= Element ->
  NewValue = min(Right),
  NewRight = delete(NewValue, Right),
  NewTree = #avl_tree{
    size = Size - 1,
    value = NewValue,
    left = Left,
    right = NewRight
  },
  balance(NewTree);
delete(Element, Tree = #avl_tree{size = Size, value = Value, right = Right}) when Value < Element ->
  NewRight = delete(Element, Right),
  NewTree = Tree#avl_tree{
    size = Size - Right#avl_tree.size + safe_size(NewRight),
    right = NewRight
  },
  balance(NewTree);
delete(Element, Tree = #avl_tree{size = Size, value = Value, left = Left}) when Value > Element ->
  NewLeft = delete(Element, Left),
  NewTree = Tree#avl_tree{
    size = Size - Left#avl_tree.size + safe_size(NewLeft),
    left = NewLeft
  },
  balance(NewTree).

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
  NewTreeSize = (Tree#avl_tree.left)#avl_tree.size,
  Q = (Tree#avl_tree.left)#avl_tree{size = Tree#avl_tree.size},
  NewTree = Tree#avl_tree{size = NewTreeSize, left = Q#avl_tree.right},
  fix_height(Q#avl_tree{right = fix_height(NewTree)}).

rotate_left(Tree) ->
  NewTreeSize = (Tree#avl_tree.right)#avl_tree.size,
  Q = (Tree#avl_tree.right)#avl_tree{size = Tree#avl_tree.size},
  NewTree = Tree#avl_tree{size = NewTreeSize, right = Q#avl_tree.left},
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

safe_size(undefined) ->
  0;
safe_size(Tree) ->
  Tree#avl_tree.size.