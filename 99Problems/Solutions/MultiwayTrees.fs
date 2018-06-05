namespace Solutions

open System

module MultiwayTrees =

    type 'a mult_tree = T of 'a * 'a mult_tree list

    //70C
    let rec count_nodes (T(_, children): 'a mult_tree): int =
        children
        |> List.fold (fun (accumulator: int) (child: 'a mult_tree) -> accumulator + count_nodes child) 1

    //71
    let rec ipl (T(_, children): 'a mult_tree): int =
        let rec path_lengths (T(_, inner_children): 'a mult_tree) (level: int): int =
            inner_children
            |> List.fold (fun (accumulator: int) (child: 'a mult_tree) -> accumulator + path_lengths child (level + 1)) 0
        children
        |> List.fold (fun (accumulator: int) (child: 'a mult_tree) -> accumulator + path_lengths child 1) 0

    //72
    let rec bottom_up (T(node, children): 'a mult_tree): 'a list =
        let rec bottom_up_internal (T(node, inner_children): 'a mult_tree) (currentList: 'a list): 'a list =
            node::currentList
            |> List.foldBack (fun (child: 'a mult_tree) (accumulator: 'a list) -> bottom_up_internal child accumulator) inner_children
        node::[]
        |> List.foldBack (fun (child: 'a mult_tree) (accumulator: 'a list) -> bottom_up_internal child accumulator) children

    //73
    let rec lispy (tree: 'a mult_tree): string =
        let rec flatten (children: 'a mult_tree list): string =
            children
            |> List.map (fun (child: 'a mult_tree) -> lispy child)
            |> String.concat " "
        match tree with
        | T(node: 'a, []) -> node.ToString()
        | T(node: 'a, children: 'a mult_tree list) -> [|"("; node.ToString(); (flatten children); ")"|] |> String.concat ""

