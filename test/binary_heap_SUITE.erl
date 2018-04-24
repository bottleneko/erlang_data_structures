-module(binary_heap_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    empty_heap_test,
    new_identity_test,
    heap_property_test,
    desc_order_heap_property_test,
    from_list_test,
    to_list_test,
    size_test,
    merge_test
  ].

empty_heap_test(_Config) ->
  ?assertEqual(undefined, binary_heap:peek(binary_heap:new())).

new_identity_test(_Config) ->
  ?assertEqual(binary_heap:new(), binary_heap:new()),
  ?assertEqual(
    binary_heap:insert(1, binary_heap:new()),
    binary_heap:insert(1, binary_heap:new())).

heap_property_test(_Config) ->
  TestList = [1,3,2,4,6,5],
  SortedList = lists:sort(TestList),
  Heap = lists:foldl(
    fun(Elem, Heap) ->
      binary_heap:insert(Elem, Heap)
    end, binary_heap:new(), TestList),
  {HeapMinimums, _} = lists:foldl(
    fun(_, {Acc, CurrentHeap}) ->
      {Min, NewHeap} = binary_heap:extract_peek(CurrentHeap),
      {[Min|Acc], NewHeap}
    end, {[], Heap}, lists:seq(1, binary_heap:size(Heap))),
  ?assertEqual(SortedList, lists:reverse(HeapMinimums)).

desc_order_heap_property_test(_Config) ->
  TestList = [1,3,2,4,6,5],
  SortedList = lists:sort(TestList),
  Heap = lists:foldl(
    fun(Elem, Heap) ->
      binary_heap:insert(Elem, Heap)
    end, binary_heap:new(fun(A, B) -> A > B end), TestList),
  {HeapMinimums, _} = lists:foldl(
    fun(_, {Acc, CurrentHeap}) ->
      {Min, NewHeap} = binary_heap:extract_peek(CurrentHeap),
      {[Min|Acc], NewHeap}
    end, {[], Heap}, lists:seq(1, binary_heap:size(Heap))),
  ?assertEqual(lists:reverse(SortedList), lists:reverse(HeapMinimums)).


from_list_test(_Config) ->
  TestList = [1,3,2,4,6,5],
  SortedList = lists:sort(TestList),
  Heap = binary_heap:from_list(TestList),
  {HeapMinimums, _} = lists:foldl(
    fun(_, {Acc, CurrentHeap}) ->
      {Min, NewHeap} = binary_heap:extract_peek(CurrentHeap),
      {[Min|Acc], NewHeap}
    end, {[], Heap}, lists:seq(1, binary_heap:size(Heap))),
  ?assertEqual(SortedList, lists:reverse(HeapMinimums)).

to_list_test(_Config) ->
  TestList = [1,3,2,4,6,5],
  SortedList = lists:sort(TestList),
  Heap = binary_heap:from_list(TestList),
  HeapMinimums = binary_heap:to_list(Heap),
  ?assertEqual(SortedList, HeapMinimums).

size_test(_Config) ->
  ?assertEqual(1, binary_heap:size(binary_heap:from_list([1]))),
  ?assertEqual(2, binary_heap:size(binary_heap:from_list([2,1]))),
  ?assertEqual(3, binary_heap:size(binary_heap:from_list([3,2,1]))).

merge_test(_Config) ->
  FirstHeap = binary_heap:from_list([1,2,3]),
  SecondHeap = binary_heap:from_list([6,5,4]),
  ThirdHeap = binary_heap:from_list([7,8,9]),
  FirstMergedHeap = binary_heap:merge(FirstHeap, SecondHeap),
  FinalHeap = binary_heap:merge(FirstMergedHeap, ThirdHeap),
  {HeapMinimums, _} = lists:foldl(
    fun(_, {Acc, CurrentHeap}) ->
      {Min, NewHeap} = binary_heap:extract_peek(CurrentHeap),
      {[Min|Acc], NewHeap}
    end, {[], FinalHeap}, lists:seq(1, binary_heap:size(FinalHeap))),
  ?assertEqual(lists:seq(1, 9), lists:reverse(HeapMinimums)).