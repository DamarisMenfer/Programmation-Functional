open Graph

(**************  FORD-FULKERSON  **************)


(*Search sink into a list of outedges until find sink.*)
let rec searchListOutedges graph currentPath outedges visited sink =
	match outedges with
	|[] -> []
	|edge::rest -> 
		begin match edge with
		|(label,id2) -> 
			(*If the edge still has remaining flow, search augmenting path using this edge.*)
			if label > 0 then
				(*If the node has already been visited, don't search again (avoid loop in the search).*)
				if (List.mem id2 visited) then searchListOutedges graph currentPath rest visited sink
				(*Else, try to find path using the vertex.*)
				else 
					begin match findAugmentingPath graph id2 visited sink with
					(*If there isn't any path, keeps the search with the rest of the outedges list.*)
					|[] -> searchListOutedges graph currentPath rest (id2::visited) sink
					(*If there is a path, return the path.*)
					|path -> if id2 = sink then path else id2::path
					end
			(*Else, the edge doens't have remaining flow, so keep the search with the rest of the list of outedges.*)
			else
				searchListOutedges graph currentPath rest visited sink
		end

(*Find a augmenting path in the graph and return a list with the vertices of the augmenting path.*)
and findAugmentingPath graph current visited sink =
	(*If the current node is the sink, return path iqual to sink. Indicates that the search ended.*)
	if current = sink then [sink]
	else	
		(*If the current node isn't the sink, keep search path in the outedges of the current vertex until find sink.*)
		let vertex = find_vertex graph current in
		searchListOutedges graph [current] vertex.outedges (current::visited) sink;;

(*Returns the label of the first edge of the augmenting path.*)
let getFirstEdge graph path = 
	match path with
	|[] -> failwith ("Error in augmenting path.")
	|[x] -> failwith ("Error in augmenting path.")
	|id1::id2::rest -> 
		begin match find_edge graph id1 id2 with
		|Some e -> e
		(*If we dont find an edge between the two vertices, there is a problem with the augmenting path.*)
		|None -> failwith ("Error in augmenting path.")
		end;;


(*Find the value of maximum flow of the augmenting path.*)
let maxGraphFlow graph path =
	let rec loop graph path maxFlow =
		match path with
		|[] -> maxFlow
		|[x] -> maxFlow
		|id1::id2::rest ->
			(*If the edge flow is smaller then max flow, make max flow equal to edge flow.*)
			(*Else, keep with the current max flow.*)
			let edgeFlow = getFirstEdge graph path in
				if edgeFlow < maxFlow then loop graph (id2::rest) edgeFlow
				else loop graph (id2::rest) maxFlow
	in
	loop graph path (getFirstEdge graph path);;

(*Make part of the actualisation of the residual graph.*)
let add_edges_residual_graph graph id1 id2 maxFlow =
	match find_edge graph id1 id2 with
	|Some edge_source_to_sink ->
		begin match find_edge graph id2 id1 with
			(*If there is already an return edge in the flow graph ->*)
			(*add max flow in the return edge (augment capacity of pushing back flow)*)
			|Some edge_sink_to_source -> add_edge graph id2 id1 (maxFlow+edge_sink_to_source)
			(*Else, if there isn't an return edge in the flow graph, add this new edge with the value of max flow.*)
			|None -> add_edge graph id2 id1 maxFlow
		end;
		(*Actualise the flow of the edge in the path, in this case make decrease the current value of the edge with max flow.*)
		add_edge graph id1 id2 (edge_source_to_sink-maxFlow);
		graph
	|None ->
		failwith ("Error in augmenting path.");;

(*Take the actual residual graph and actualise it with the given augmenting path.*)
let actualiseResidualGraph graph path maxFlow =
	let rec loop graph path maxFlow =
		(*Actualise the value of the edges used in the augmenting path. At the end, return graph actualized.*) 
		match path with
		|[] -> graph
		|[x] -> graph
		|id1::id2::rest ->
			(*Actualise/Add values of two edges in the residual graph.*)
			let graph1 = add_edges_residual_graph graph id1 id2 maxFlow in
			loop graph1 (id2::rest) maxFlow
	in
	loop graph path maxFlow;;

(*Take the actual graph flow and actualise it with the given augmenting path.*)
let actualiseFlowGraph flowGraph path maxFlow =
	let rec loop flowGraph path maxFlow = 
		match path with
		|[] -> flowGraph
		|[x] -> flowGraph
		|id1::id2::rest ->
			begin match find_edge flowGraph id1 id2 with
				(*If there is alread an edge from id1 to id2, so add flow in this edge.*)
				|Some e -> add_edge flowGraph id1 id2 (e+maxFlow)
				|None -> 
					(*Else, that means that the augmenting path caught an return edge (pushed flow back)*)
					(*In this case, decrease the flow in the edge.*)
					begin match find_edge flowGraph id2 id1 with
					|Some e -> add_edge flowGraph id2 id1 (e-maxFlow)
					|None -> failwith ("Error in flow graph.")
					end;
			end;
			(*Keep the updating of the graph until the end of the path.*)
			loop flowGraph (id2::rest) maxFlow
	in
	loop flowGraph path maxFlow;;

(*Take a graph a source and a sink and return the maximum flow graph.*)
let fordFulkerson graph source sink =
	(*Creates the flowGraph with 0 flow in the begining.*)
	let flowGraph = map graph (fun x -> x) (fun x -> 0) in
	(*While there is a augmenting path in the residual graph, augment flow.*)
	(*Else, return flow graph.*)
	let rec loop graph source sink flowGraph =
		match (source::findAugmentingPath graph source [] sink) with
		|[source] -> flowGraph
		|path -> 
			(*Search the maximum flow in the augmenting path.*)
			let maxFlow = maxGraphFlow graph path in
			(*Actualise residual graph and flow graph and keep trying augmenting the flow.*)
			loop (actualiseResidualGraph graph path maxFlow) source sink (actualiseFlowGraph flowGraph path maxFlow)
	in
	(*Start with 0 flow in the graph flow.*)
	loop graph source sink flowGraph;;

	








