namespace Solutions

open System

module Graph =

    /// <summary>Represents an undirected edge between two nodes.</summary>
    /// <typeparam name="'a">Node type.</typeparam>
    type 'a edge = { node1: 'a; node2: 'a }

    /// <summary>List of edges.</summary>
    /// <typeparam name="'a">Node type.</typeparam>
    type 'a edge_list = 'a edge list

    /// <summary>Graph term representation containing a list of nodes and a list of edges.</summary>
    /// <typeparam name="'a">Node type.</typeparam>
    type 'a grapth_term = { nodes: 'a list; edges: 'a edge_list }

    /// <summary>Adjacency entry for a single node and its neighbours.</summary>
    /// <typeparam name="'a">Node type.</typeparam>
    type 'a adjacency = { node: 'a; neighbours: 'a list }

    /// <summary>List of adjacency entries representing a graph.</summary>
    /// <typeparam name="'a">Node type.</typeparam>
    type 'a adjacency_list = 'a adjacency list

    /// <summary>Human-friendly element: either a node or an edge.</summary>
    /// <typeparam name="'a">Node type.</typeparam>
    type 'a element =
        | Node of 'a
        | Edge of 'a edge

    /// <summary>Human-friendly representation as a list of nodes and edges.</summary>
    /// <typeparam name="'a">Node type.</typeparam>
    type 'a human_friendly = 'a element list

    // 80
    /// <summary>Constructs a graph term from an edge list.</summary>
    /// <param name="edges">List of edges.</param>
    /// <returns>A graph term with distinct nodes and the provided edges.</returns>
    let from_edge_list_to_graph_term (edges: 'a edge_list): 'a grapth_term =
        let nodes =
            edges
            |> List.collect (fun edge -> [edge.node1; edge.node2])
            |> List.distinct
        { nodes = nodes; edges = edges }

    /// <summary>Converts an edge list into an adjacency list.</summary>
    /// <param name="edges">List of edges.</param>
    /// <returns>An adjacency list where each node lists its neighbours.</returns>
    let from_edge_list_to_adjacency_list (edges: 'a edge_list): 'a adjacency_list =
        let rec add_edge (adjacencyList: 'a adjacency_list) (edge: 'a edge): 'a adjacency_list =
            let update_neighbours (node: 'a) (neighbour: 'a) =
                match List.tryFind (fun adj -> adj.node = node) adjacencyList with
                | Some adj -> { adj with neighbours = neighbour :: adj.neighbours }
                | None -> { node = node; neighbours = [neighbour] }
            let updatedAdjacencyList = update_neighbours edge.node1 edge.node2
            let updatedAdjacencyList' = update_neighbours edge.node2 edge.node1
            updatedAdjacencyList :: List.filter (fun adj -> adj.node <> edge.node1 && adj.node <> edge.node2) adjacencyList @ [updatedAdjacencyList']
        List.fold add_edge [] edges

    /// <summary>Wraps an edge list into a human-friendly representation (as edges only).</summary>
    /// <param name="edges">List of edges.</param>
    /// <returns>Human-friendly list with each edge wrapped as <c>Edge</c>.</returns>
    let from_edge_list_to_human_friendly (edges: 'a edge_list): 'a human_friendly =
        edges
        |> List.map Edge

    /// <summary>Extracts the edge list from a graph term.</summary>
    /// <param name="graphTerm">Graph term.</param>
    /// <returns>The edge list contained in the graph term.</returns>
    let from_graph_term_to_edge_list (graphTerm: 'a grapth_term): 'a edge_list =
        graphTerm.edges

    /// <summary>Converts a graph term to an adjacency list.</summary>
    /// <param name="graphTerm">Graph term.</param>
    /// <returns>Adjacency list equivalent of the graph term.</returns>
    let from_graph_term_to_adjacency_list (graphTerm: 'a grapth_term): 'a adjacency_list =
        from_edge_list_to_adjacency_list graphTerm.edges

    /// <summary>Converts a graph term to a human-friendly list of nodes and edges.</summary>
    /// <param name="graphTerm">Graph term.</param>
    /// <returns>Human-friendly list containing nodes as <c>Node</c> and edges as <c>Edge</c>.</returns>
    let from_graph_term_to_human_friendly ({ nodes = nodes; edges = edges }: 'a grapth_term): 'a human_friendly =
        let nodes_human_friendly = nodes |> List.map Node
        let edges_human_friendly = edges |> List.map Edge
        nodes_human_friendly @ edges_human_friendly

    /// <summary>Converts an adjacency list into an edge list.</summary>
    /// <param name="adjacencyList">Adjacency list.</param>
    /// <returns>Edge list with distinct edges (ordered by node pair).</returns>
    let from_adjacency_list_to_edge_list (adjacencyList: 'a adjacency_list): 'a edge_list =
        adjacencyList
        |> List.collect (fun adj -> adj.neighbours |> List.map (fun neighbour -> { node1 = adj.node; node2 = neighbour }))
        |> List.distinctBy (fun edge -> (edge.node1, edge.node2))

    /// <summary>Converts an adjacency list to a graph term.</summary>
    /// <param name="adjacencyList">Adjacency list.</param>
    /// <returns>Graph term with nodes derived from adjacency entries and edges derived from neighbours.</returns>
    let from_adjacency_list_to_graph_term (adjacencyList: 'a adjacency_list): 'a grapth_term =
        let edges = from_adjacency_list_to_edge_list adjacencyList
        let nodes = adjacencyList |> List.map (fun adj -> adj.node) |> List.distinct
        { nodes = nodes; edges = edges }

    /// <summary>Converts an adjacency list to a human-friendly list of nodes and edges.</summary>
    /// <param name="adjacencyList">Adjacency list.</param>
    /// <returns>Human-friendly list containing <c>Node</c> entries for each adjacency node and <c>Edge</c> entries for each neighbour pair.</returns>
    let from_adjacency_list_to_human_friendly (adjacencyList: 'a adjacency_list): 'a human_friendly =
        let nodes = adjacencyList |> List.map (fun adj -> Node adj.node)
        let edges = adjacencyList |> List.collect (fun adj -> adj.neighbours |> List.map (fun neighbour -> Edge { node1 = adj.node; node2 = neighbour }))
        nodes @ edges

    /// <summary>Extracts the edge list from a human-friendly representation.</summary>
    /// <param name="humanFriendly">Human-friendly list.</param>
    /// <returns>List of edges found in the human-friendly representation.</returns>
    let from_human_friendly_to_edge_list (humanFriendly: 'a human_friendly): 'a edge_list =
        humanFriendly
        |> List.choose (function
            | Edge edge -> Some edge
            | Node _ -> None)

    /// <summary>Converts a human-friendly list to a graph term.</summary>
    /// <param name="humanFriendly">Human-friendly list.</param>
    /// <returns>Graph term where nodes are distinct <c>Node</c> entries and edges are extracted from <c>Edge</c> entries.</returns>
    let from_human_friendly_to_graph_term (humanFriendly: 'a human_friendly): 'a grapth_term =
        let edges = from_human_friendly_to_edge_list humanFriendly
        let nodes =
            humanFriendly
            |> List.choose (function
                | Node node -> Some node
                | Edge _ -> None)
            |> List.distinct
        { nodes = nodes; edges = edges }

    /// <summary>Converts a human-friendly list to an adjacency list.</summary>
    /// <param name="humanFriendly">Human-friendly list.</param>
    /// <returns>Adjacency list generated from edges found in the human-friendly list.</returns>
    let from_human_friendly_to_adjacency_list (humanFriendly: 'a human_friendly): 'a adjacency_list =
        let edges = from_human_friendly_to_edge_list humanFriendly
        from_edge_list_to_adjacency_list edges

    // 81

    /// <summary>Finds all simple paths between two nodes given an edge list.</summary>
    /// <param name="edgeList">Edge list.</param>
    /// <param name="startNode">Start node.</param>
    /// <param name="endNode">End node.</param>
    /// <returns>List of paths, each path represented as a node list from start to end.</returns>
    let paths_edge_list (edgeList: 'a edge_list) (startNode: 'a) (endNode: 'a): 'a list list =
        let adjacencyList = from_edge_list_to_adjacency_list edgeList
        paths_adjacency_list adjacencyList startNode endNode

    /// <summary>Finds all simple paths between two nodes in a graph term.</summary>
    /// <param name="graphTerm">Graph term.</param>
    /// <param name="startNode">Start node.</param>
    /// <param name="endNode">End node.</param>
    /// <returns>List of paths as node lists.</returns>
    let paths_graph_term (graphTerm: 'a grapth_term) (startNode: 'a) (endNode: 'a): 'a list list =
        let adjacencyList = from_graph_term_to_adjacency_list graphTerm
        paths_adjacency_list adjacencyList startNode endNode

    /// <summary>Finds all simple paths between two nodes given an adjacency list.</summary>
    /// <param name="adjacencyList">Adjacency list.</param>
    /// <param name="startNode">Start node.</param>
    /// <param name="endNode">End node.</param>
    /// <returns>All simple paths from start to end as lists of nodes.</returns>
    let paths_adjacency_list (adjacencyList: 'a adjacency_list) (startNode: 'a) (endNode: 'a): 'a list list =
        let rec dfs (currentNode: 'a) (endNode: 'a) (visited: Set<'a>) (path: 'a list): 'a list list =
            if currentNode = endNode then
                [List.rev (currentNode :: path)]
            else
                let neighbours =
                    match List.tryFind (fun adj -> adj.node = currentNode) adjacencyList with
                    | Some adj -> adj.neighbours
                    | None -> []
                neighbours
                |> List.filter (fun neighbour -> not (Set.contains neighbour visited))
                |> List.collect (fun neighbour ->
                    dfs neighbour endNode (Set.add currentNode visited) (currentNode :: path))
        dfs startNode endNode Set.empty []

    /// <summary>Finds all simple paths between two nodes in a human-friendly representation.</summary>
    /// <param name="humanFriendly">Human-friendly graph.</param>
    /// <param name="startNode">Start node.</param>
    /// <param name="endNode">End node.</param>
    /// <returns>List of paths as node lists.</returns>
    let paths_human_friendly (humanFriendly: 'a human_friendly) (startNode: 'a) (endNode: 'a): 'a list list =
        let adjacencyList = from_human_friendly_to_adjacency_list humanFriendly
        paths_adjacency_list adjacencyList startNode endNode

    // 82

    /// <summary>Finds all cycles starting and ending at a node using an edge list.</summary>
    /// <param name="edgeList">Edge list.</param>
    /// <param name="startNode">Start node for cycles.</param>
    /// <returns>List of cycles, each cycle is a list of nodes (start node appears at both ends logically).</returns>
    let cycle_edge_list (edgeList: 'a edge_list) (startNode: 'a): 'a list list =
        let adjacencyList = from_edge_list_to_adjacency_list edgeList
        cycles_adjacency_list adjacencyList startNode

    /// <summary>Finds all cycles starting and ending at a node using a graph term.</summary>
    /// <param name="graphTerm">Graph term.</param>
    /// <param name="startNode">Start node for cycles.</param>
    /// <returns>List of cycles as node lists.</returns>
    let cycle_graph_term (graphTerm: 'a grapth_term) (startNode: 'a): 'a list list =
        let adjacencyList = from_graph_term_to_adjacency_list graphTerm
        cycles_adjacency_list adjacencyList startNode

    /// <summary>Finds all cycles that begin and end at a given node in an adjacency list.</summary>
    /// <param name="adjacencyList">Adjacency list.</param>
    /// <param name="startNode">Start node for cycles.</param>
    /// <returns>All cycles as lists of nodes.</returns>
    let cycles_adjacency_list (adjacencyList: 'a adjacency_list) (startNode: 'a): 'a list list =
        let rec dfs (currentNode: 'a) (startNode: 'a) (visited: Set<'a>) (path: 'a list): 'a list list =
            if currentNode = startNode && not (Set.isEmpty visited) then
                [List.rev (currentNode :: path)]
            else
                let neighbours =
                    match List.tryFind (fun adj -> adj.node = currentNode) adjacencyList with
                    | Some adj -> adj.neighbours
                    | None -> []
                neighbours
                |> List.filter (fun neighbour -> not (Set.contains neighbour visited) || neighbour = startNode)
                |> List.collect (fun neighbour ->
                    dfs neighbour startNode (Set.add currentNode visited) (currentNode :: path))
        dfs startNode startNode Set.empty []

    /// <summary>Finds all cycles for a graph represented in a human-friendly format.</summary>
    /// <param name="humanFriendly">Human-friendly graph.</param>
    /// <param name="startNode">Start node for cycles.</param>
    /// <returns>List of cycles as node lists.</returns>
    let cycles_human_friendly (humanFriendly: 'a human_friendly) (startNode: 'a): 'a list list =
        let adjacencyList = from_human_friendly_to_adjacency_list humanFriendly
        cycles_adjacency_list adjacencyList startNode

    