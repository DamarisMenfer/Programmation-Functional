open Graph

(**************  TRANSFORMATION  **************)

(* Read a graph of a bipartite matching problem from a file ,
 * Write a graph to a file. *)

type path = string

(* Values are read as strings. *)
val from_file: path -> (string, string) graph
