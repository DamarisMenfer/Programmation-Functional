open Graph

(**************  TRANSFORMATION  **************)

(* Read a graph of a bipartite matching problem from a file ,
 * Write a graph to a file. *)

type path = string

(* Reads a line with two vertex (group and project) and add them to the graph. *)
let read_vertex graph line =
  try Scanf.sscanf line "%s %s" (fun id1 id2 -> add_vertex graph "3" id1; add_vertex graph "4" id2)
  with e -> Printf.printf "Cannot read vertex in line - %s:\n%s\n" (Printexc.to_string e) line



(* Reads a line with two vertex that represents one option of project to the given group. Add one vertex between the group and the option, another vertex between the group and the source and another between the project and the target. *)
let read_edge graph line =
  try Scanf.sscanf line "%s %s" (fun id1 id2 -> add_edge graph id1 id2 "1"; add_edge graph "s" id1 "1"; add_edge graph id2 "t" "1")
  with e -> Printf.printf "Cannot read edge in line - %s:\n%s\n" (Printexc.to_string e) line

let from_file path =

  let infile = open_in path in

  (* Create new graph *)
  let graph = new_graph () in
  add_vertex graph "1" "s";
  add_vertex graph "2" "t";

  (* Populate it *)
  let rec loop () =
    try
      let line = input_line infile in
      let () =
        if line = "" then ()
        else match line with
          | "fin" -> () 
          | _ -> read_vertex graph line; read_edge graph line
      in                 
      loop ()        
    with End_of_file -> ()
  in

  loop () ;
  
  close_in infile ;
  graph
  
    
