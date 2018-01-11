open Graph

(**************  FORD-FULKERSON  **************)

val getFirstEdge: ('v1, int) graph -> id list -> int

val findAugmentingPath: ('v1, int) graph -> id -> id list -> id -> id list

val maxGraphFlow: ('v1, int) graph -> id list -> int

val add_edges_residual_graph: ('v1, int) graph -> id -> id -> int -> ('v1, int) graph

val actualiseResidualGraph: ('v1, int) graph -> id list-> int -> ('v1, int) graph

val actualiseFlowGraph: ('v1, int) graph -> id list -> int -> ('v1, int) graph

val fordFulkerson: ('v1, int) graph -> id -> id -> ('v1, int) graph


