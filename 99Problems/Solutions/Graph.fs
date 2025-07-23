namespace Solutions

open System

module Graph =

    type 'a edge = { node1: 'a; node2: 'a }

    type 'a edge_list = 'a edge list

    type 'a grapth_term = { nodes: 'a list; edges: 'a edge_list }

    type 'a adjacency = { node: 'a; neighbours: 'a list }

    type 'a adjacency_list = 'a adjacency list

    type 'a element =
        | Node of 'a
        | Edge of 'a edge

    type 'a human_friendly = 'a element list