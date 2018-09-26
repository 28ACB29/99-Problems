namespace Solutions

open System

module BinaryTrees =

    type 'a binary_tree =
        | Empty
        | Node of 'a * 'a binary_tree * 'a binary_tree

    let cons (head: 'a) (tail: 'a list) = head::tail

    let create_trees (leftList: string binary_tree list) (rightList: string binary_tree list) (currentList: string binary_tree list): string binary_tree list =
        List.fold (fun (fullList: string binary_tree list) (leftTree: string binary_tree) -> List.fold (fun (accumulator: string binary_tree list) (rightTree: string binary_tree) -> Node("x", leftTree, rightTree)::accumulator) fullList rightList) currentList leftList

    //55
    let rec cbal_tree (n: int): string binary_tree list =
        let create_trees_even (n_even: int): string binary_tree list =
            let leftList: string binary_tree list = cbal_tree (n_even / 2)
            let rightList: string binary_tree list = cbal_tree ((n_even - 1) / 2)
            []
            |> create_trees rightList leftList
            |> create_trees leftList rightList
        let create_trees_odd (n_odd: int): string binary_tree list =
            let treeList: string binary_tree list = cbal_tree ((n_odd - 1) / 2)
            []
            |> create_trees treeList treeList
        match n % 2 with
        | 0 -> create_trees_even n
        | 1 -> create_trees_odd n
        | _ -> []

    //56
    let is_mirror (tree: 'a binary_tree): bool =
        let rec is_symmetric (leftTree: 'a binary_tree) (righttTree: 'a binary_tree): bool =
            match leftTree, righttTree with
            | Empty, Empty -> true
            | Node(_, leftLeft: 'a binary_tree, leftRight: 'a binary_tree), Node(_, rightLeft: 'a binary_tree, rightRight: 'a binary_tree) -> (is_symmetric leftLeft rightRight) && (is_symmetric leftRight rightLeft)
            | _-> false
        match tree with
        | Empty -> true
        | Node(_, left: 'a binary_tree, right: 'a binary_tree) -> is_symmetric left right

    //57
    let construct (integers: 'a list): 'a binary_tree =
        let rec insert (number: 'a) (tree: 'a binary_tree) =
            match number, tree with
            | number, Empty -> Node(number, Empty, Empty)
            | number, Node(value: 'a, _, _) when number = value -> tree
            | number, Node(value: 'a, left: 'a binary_tree, right: 'a binary_tree) when number < value -> Node(value, insert number left, right)
            | number, Node(value: 'a, left: 'a binary_tree, right: 'a binary_tree) when number > value -> Node(value, left, insert number right)
            | _ -> Empty
        List.fold (fun accumulator element -> insert element accumulator) Empty integers

    //58
    let sym_bal_trees (n: int): string binary_tree list =
        n
        |> cbal_tree
        |> List.filter is_mirror

    //59
    let rec hbal_tree (n: int): string binary_tree list =
        let create_trees_all (height: int): string binary_tree list =
            let largerList: string binary_tree list = hbal_tree (height - 1)
            let smallerList: string binary_tree list = hbal_tree (height - 2)
            []
            |> create_trees smallerList largerList
            |> create_trees largerList smallerList
            |> create_trees largerList largerList
        match n with
        | 0 -> Empty::[]
        | 1 -> Node("x", Empty, Empty)::[]
        | _ -> create_trees_all n

    //61
    let rec count_leaves (tree: 'a binary_tree): int =
        match tree with
        | Empty -> 0
        | Node(_, Empty, Empty) -> 1
        | Node(_, left: 'a binary_tree, right: 'a binary_tree) -> (count_leaves left) + (count_leaves right)

    //61A
    let leaves (tree: 'a binary_tree): 'a list =
        let rec leaves_internal (internalTree: 'a binary_tree) (currentList: 'a list): 'a list =
            match internalTree with
            | Node(value: 'a, Empty, Empty) -> value::[]
            | Node(_, left: 'a binary_tree, right: 'a binary_tree) ->
                currentList
                |> leaves_internal right
                |> leaves_internal left
            | _ -> []
        match tree with
        | Empty -> []
        | _ -> leaves_internal tree []

    //62
    let internals (tree: 'a binary_tree): 'a list =
        let rec internals_internal (internalTree: 'a binary_tree) (currentList: 'a list): 'a list =
            match internalTree with
            | Node(value : 'a, left: 'a binary_tree, right: 'a binary_tree) when left <> Empty && right <> Empty ->
                currentList
                |> internals_internal right
                |> cons value
                |> internals_internal left
            | _ -> []
        match tree with
        | Node(_, left: 'a binary_tree, right: 'a binary_tree) when left <> Empty && right <> Empty -> internals_internal tree []
        | _ -> []

    //62B
    let rec at_level (tree: 'a binary_tree) (level: int): 'a list =
        match tree, level with
        | Node(value: 'a, _, _), 1 -> value::[]
        | Node(_, left: 'a binary_tree, right: 'a binary_tree), level when level > 1 -> (at_level left (level - 1)) @ (at_level right (level - 1))
        | _ -> []

    //67
    let rec string_of_tree (tree: 'a binary_tree): string =
        let rec string_of_tree_internal (tree: 'a binary_tree): string =
            match tree with
            | Empty -> ""
            | Node(value: 'a, left: 'a binary_tree, right: 'a binary_tree) ->
                [|value.ToString(); "("; string_of_tree_internal left; ","; string_of_tree_internal right; ")"|]
                |> String.concat ""
        string_of_tree_internal tree

    //68
    let preorder (tree: 'a binary_tree): 'a list =
        let rec preorder_internal (internalTree: 'a binary_tree) (currentList: 'a list): 'a list =
            match internalTree with
            | Empty -> []
            | Node(value : 'a, left: 'a binary_tree, right: 'a binary_tree) ->
                currentList
                |> preorder_internal right
                |> preorder_internal left
                |> cons value
        match tree with
        | Empty -> []
        | Node(_, _, _) -> preorder_internal tree []

    //68
    let inorder (tree: 'a binary_tree): 'a list =
        let rec inorder_internal (internalTree: 'a binary_tree) (currentList: 'a list): 'a list =
            match internalTree with
            | Empty -> []
            | Node(value : 'a, left: 'a binary_tree, right: 'a binary_tree) ->
                currentList
                |> inorder_internal right 
                |> cons value
                |> inorder_internal left
        match tree with
        | Empty -> []
        | Node(_, _, _) -> inorder_internal tree []

    //68
    let postorder (tree: 'a binary_tree): 'a list =
        let rec postorder_internal (internalTree: 'a binary_tree) (currentList: 'a list): 'a list =
            match internalTree with
            | Empty -> []
            | Node(value : 'a, left: 'a binary_tree, right: 'a binary_tree) ->
                currentList
                |> cons value
                |> postorder_internal right
                |> postorder_internal left
        match tree with
        | Empty -> []
        | Node(_, _, _) -> postorder_internal tree []

