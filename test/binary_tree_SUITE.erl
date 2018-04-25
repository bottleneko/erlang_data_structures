-module(binary_tree_SUITE).

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
    length_test
  ].

new_identity_test(_Config) ->
  ?assertEqual(binary_tree:new(), binary_tree:new()).

insert_test(_Config) ->
  Tree = binary_tree:insert(1, binary_tree:new()),
  ?assertEqual(1, binary_tree:max(Tree)),
  ?assertEqual(1, binary_tree:min(Tree)).

extended_insert_test(_Config) ->
  Tree = lists:foldl(
    fun(Elem, Acc) ->
      binary_tree:insert(Elem, Acc)
    end, binary_tree:new(), [1,2,3]),
  ?assertEqual(3, binary_tree:max(Tree)),
  ?assertEqual(1, binary_tree:min(Tree)).

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
    end, {[], Tree}, lists:seq(1, binary_tree:length(Tree))),
  ?assertEqual(lists:reverse(lists:sort(List)), SortedList).

maximums_test(_Config) ->
  List = [0,4,5,1,2,3],
  Tree = lists:foldl(
    fun(Elem, Acc) ->
      binary_tree:insert(Elem, Acc)
    end, binary_tree:new(), List),
  {SortedList, _} = lists:foldl(
    fun(_Elem, {Acc, AccTree}) ->
      Max = binary_tree:max(AccTree),
      NewTree = binary_tree:delete(Max, AccTree),
      {[Max|Acc], NewTree}
    end, {[], Tree}, lists:seq(1, binary_tree:length(Tree))),
  ?assertEqual(lists:sort(List), SortedList).

length_test(_Config) ->
  Tree = lists:foldl(
    fun(Elem, Acc) ->
      binary_tree:insert(Elem, Acc)
    end, binary_tree:new(), [1,2,3]),
  ?assertEqual(3, binary_tree:length(Tree)).