-module(avl_tree_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    new_identity_test,
    insert_test,
    extended_insert_test,
    minimums_test,
    maximums_test,
    size_test
  ].

new_identity_test(_Config) ->
  ?assertEqual(avl_tree:new(), avl_tree:new()).

insert_test(_Config) ->
  Tree = avl_tree:insert(1, avl_tree:new()),
  ?assertEqual(1, avl_tree:max(Tree)),
  ?assertEqual(1, avl_tree:min(Tree)).

extended_insert_test(_Config) ->
  Tree = lists:foldl(
    fun(Elem, Acc) ->
      avl_tree:insert(Elem, Acc)
    end, avl_tree:new(), [1,2,3]),
  ?assertEqual(3, avl_tree:max(Tree)),
  ?assertEqual(1, avl_tree:min(Tree)).

minimums_test(_Config) ->
  List = [0,4,5,1,2,3],
  Tree = lists:foldl(
    fun(Elem, Acc) ->
      avl_tree:insert(Elem, Acc)
    end, avl_tree:new(), List),
  {SortedList, _} = lists:foldl(
    fun(_Elem, {Acc, AccTree}) ->
      Min = avl_tree:min(AccTree),
      NewTree = avl_tree:delete(Min, AccTree),
      {[Min|Acc], NewTree}
    end, {[], Tree}, lists:seq(1, avl_tree:size(Tree))),
  ?assertEqual(lists:reverse(lists:sort(List)), SortedList).

maximums_test(_Config) ->
  List = [0,4,5,1,2,3],
  Tree = lists:foldl(
    fun(Elem, Acc) ->
      avl_tree:insert(Elem, Acc)
    end, avl_tree:new(), List),
  {SortedList, _} = lists:foldl(
    fun(_Elem, {Acc, AccTree}) ->
      Max = avl_tree:max(AccTree),
      NewTree = avl_tree:delete(Max, AccTree),
      {[Max|Acc], NewTree}
    end, {[], Tree}, lists:seq(1, avl_tree:size(Tree))),
  ?assertEqual(lists:sort(List), SortedList).


size_test(_Config) ->
  Tree = lists:foldl(
    fun(Elem, Acc) ->
      avl_tree:insert(Elem, Acc)
    end, avl_tree:new(), [1,2,3,4,5,6]),
  ?assertEqual(6, avl_tree:size(Tree)).