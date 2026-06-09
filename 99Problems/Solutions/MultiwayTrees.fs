namespace Solutions

open System

module MultiwayTrees =

    type 'a mult_tree = T of 'a * 'a mult_tree list

    //70A
    let string_of_tree (tree: 'a mult_tree): string =
        let string_of_tree_internal (builder: StringBuilder) ((node: 'a, children: 'a mult_tree list): 'a mult_tree): string =
            builder.Append(node.ToString())
            List.fold (fun (acc, child: 'a mult_tree) -> string_of_tree_internal acc child) builder children
            builder.Append("^")
            builder
        let builder: StringBuilder = StringBuilder()
        string_of_tree_internal builder tree
        builder.ToString()

    let tree_of_string (s: string): 'a mult_tree =
        let rec tree_of_string_internal (stack: 'a mult_tree list) (currentNode: string) (index: int): 'a mult_tree =
            match index with
            | i when i >= s.Length -> stack.Head
            | i when s.[i] = '^' ->
                let node = T(currentNode, List.rev stack)
                tree_of_string_internal [] "" (i + 1)
            | i -> tree_of_string_internal stack (currentNode + s.[i].ToString()) (i + 1)
        tree_of_string_internal [] "" 0

    //70B
    let rec count_nodes ((_, children: 'a mult_tree list): 'a mult_tree): int =
        children
        |> List.fold (fun (accumulator: int) (child: 'a mult_tree) -> accumulator + count_nodes child) 1

    //70C
    let bottom_up (tree: 'a mult_tree): 'a list =
        let rec bottom_up_internal (stack: 'a list) ((node: 'a, children: 'a mult_tree list): 'a mult_tree): 'a list =
            List.foldback (fun (child: 'a mult_tree) (acc: 'a list) -> bottom_up_internal acc child) children stack
            node::stack
        let stack: 'a list = []
        bottom_up_internal stack tree
        stack.ToString()

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
        | T(node: 'a, children: 'a mult_tree list) ->
            [|"("; node.ToString(); (flatten children); ")"|]
            |> String.concat ""

