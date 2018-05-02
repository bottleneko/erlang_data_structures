-module(avl_tree_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    new_identity_test,
    insert_test,
    extended_insert_test
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