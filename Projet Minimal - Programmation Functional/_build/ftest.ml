open Graph
open Fordfulkerson

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  and outfile = Sys.argv.(4) in

  let graph = Gfile.from_file infile in
    let mappedgraph1 = Graph.map graph int_of_string int_of_string in
    let mappedgraph2 = Graph.map mappedgraph1 (fun x -> x) (fun x -> x) in
    let mappedgraph3 = Fordfulkerson.fordFulkerson mappedgraph2 _source _sink in
    let mappedgraph4 = Graph.map mappedgraph3 string_of_int string_of_int in
     let () = Gfile.export outfile mappedgraph4 in
      ()

  (*
  let graph = Gfile.from_file infile in
    let () = Gfile.export outfile graph in
    ()
  *)

  (* Rewrite the graph that has been read. *)
  
  (* 
            let () = Gfile.write_file outfile graph in
            ()
  *)


