-record(binary_heap, {
  size,
  container,
  comparator
}).

-record(binary_tree, {
  size,
  value,
  left,
  right
}).

-record(avl_tree, {
  value,
  height,
  left,
  right
}).

-record(directed_graph, {
  container
}).

-record(directed_weighted_graph, {
  edge = weighted_edge,
  container
}).

-record(weighted_edge, {
  route,
  weight
}).
