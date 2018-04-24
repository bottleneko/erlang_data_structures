-module(binary_heap).

-include("data_structures.hrl").

-export([
  new/0,
  new/1,
  from_list/1,
  to_list/1,
  insert/2,
  extract_peek/1,
  peek/1,
  size/1,
  merge/2]).

-define(DEFAULT_COMPARATOR, fun(A, B) -> A < B end).

%%====================================================================
%% API functions
%%====================================================================

new() ->
  #binary_heap{size = 0, container = array:new(), comparator = ?DEFAULT_COMPARATOR}.

new(Comparator) ->
  #binary_heap{size = 0, container = array:new(), comparator = Comparator}.

from_list(List) ->
  heapify(#binary_heap{size = length(List), container = array:from_list(List), comparator = ?DEFAULT_COMPARATOR}).

to_list(Heap) ->
  {List, _} = lists:foldl(
    fun(_, {Acc, CurrentHeap}) ->
      {Min, NewHeap} = binary_heap:extract_peek(CurrentHeap),
      {[Min|Acc], NewHeap}
    end, {[], Heap}, lists:seq(1, binary_heap:size(Heap))),
  lists:reverse(List).

insert(Elem, #binary_heap{
  size = OldSize,
  container = OldContainer,
  comparator = Comparator}) ->
  NewHeap = #binary_heap{
    size = OldSize + 1,
    container = array:set(OldSize, Elem, OldContainer),
    comparator = Comparator},
  sift_up(NewHeap, OldSize).

extract_peek(#binary_heap{
  size = Size,
  container = Container,
  comparator = Comparator}) ->
  Peek = array:get(0, Container),
  NewArray = array:set(0, array:get(Size - 1, Container), Container),
  NewHeap = sift_down(#binary_heap{size = Size - 1, container = NewArray, comparator = Comparator}, 0),
  {Peek, NewHeap}.

peek(#binary_heap{container = Container}) ->
  array:get(0, Container).

size(#binary_heap{size = Size}) -> Size.

merge(FirstHeap = #binary_heap{size = FirstSize},
    #binary_heap{size = SecondSize, container = SecondContainer}) ->
  Indexes = lists:zip(
    lists:seq(FirstSize, FirstSize+SecondSize - 1),
    lists:seq(0, SecondSize - 1)
  ),
  lists:foldl(
    fun({FirstIndex, SecondIndex}, #binary_heap{size = HeapSize, container = HeapContainer, comparator = Comparator}) ->
      heapify(#binary_heap{
        size = HeapSize + 1,
        container = array:set(FirstIndex, array:get(SecondIndex, SecondContainer), HeapContainer),
        comparator = Comparator})
    end, FirstHeap, Indexes).

%%====================================================================
%% Internal functions
%%====================================================================

sift_up(Heap = #binary_heap{container = Container, comparator = Comparator}, Index) ->
  {_, NewContainer} = lists:foldl(
      fun
        ({CurrentInd, ParentInd}, {false, Array}) ->
          case Comparator(array:get(CurrentInd, Array), array:get(ParentInd, Array)) of
            true ->
              {false, swap(CurrentInd, ParentInd, Array)};
            false ->
              {true, Array}
        end;
        ({_, _}, ArrayT = {true, _Array}) ->
          ArrayT
      end, {false, Container}, sift_up_seq(Index)),
  Heap#binary_heap{container = NewContainer}.

sift_down(Heap = #binary_heap{size = Size, container = Container, comparator = Comparator}, Index) when Index*2 + 1 < Size->
  Left = Index*2 + 1,
  Right = Index*2 + 2,
  Branch =
    case Right < Size andalso
      Comparator(array:get(Right, Container), array:get(Left, Container)) of
      true ->
        Right;
      false ->
        Left
    end,
  case Comparator(array:get(Branch, Container), array:get(Index, Container)) of
    false ->
      Heap;
    true ->
      NewContainer = swap(Index, Branch, Container),
      sift_down(Heap#binary_heap{container = NewContainer}, Branch)
  end;
sift_down(Heap, _) -> Heap.

heapify(Heap = #binary_heap{size = Size}) ->
  lists:foldl(
    fun(Elem, CurrentHeap) ->
      sift_down(CurrentHeap, Elem)
    end, Heap, lists:seq(Size div 2, 0, -1)).

sift_up_seq(Index) ->
  sift_up_seq_T(Index, (Index - 1) div 2, []).

sift_up_seq_T(Index, 0, Acc) ->
  lists:reverse([{Index, 0} | Acc]);
sift_up_seq_T(Index, ParentIndex, Acc) ->
  sift_up_seq_T(ParentIndex, (ParentIndex-1) div 2, [{Index, ParentIndex}|Acc]).

swap(FirstIndex, SecondIndex, Array) ->
  Temp = array:get(FirstIndex, Array),
  WithSecondArray = array:set(FirstIndex, array:get(SecondIndex, Array), Array),
  array:set(SecondIndex, Temp, WithSecondArray).

%%====================================================================
%% EUnit tests
%%====================================================================

-ifdef(TEST).

-endif.