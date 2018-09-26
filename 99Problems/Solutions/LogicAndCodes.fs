namespace Solutions

open System

module LogicAndCodes =

    type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

    type 'a huffman_tree =
        | Leaf of ('a * int)
        | Internal of int * 'a huffman_tree * 'a huffman_tree

    //46, 47
    let table2 (variable1: string) (variable2: string) (expression: bool_expr): (bool * bool * bool) list =
        let rec eval2 (expression: bool_expr) (value1: bool) (value2: bool) =
            let substitute2 (variable: string) =
                match variable with
                | variable when variable = variable1 -> value1
                | variable when variable = variable2 -> value2
                | _-> false
            match expression with
            | Var(variable: string)  -> substitute2 variable
            | Not(inner: bool_expr) -> not (eval2 inner value1 value2)
            | And(inner1: bool_expr, inner2: bool_expr) -> (eval2 inner1 value1 value2) && (eval2 inner2 value1 value2)
            | Or(inner1: bool_expr, inner2: bool_expr) -> (eval2 inner1 value1 value2) || (eval2 inner2 value1 value2)
        [(false, false); (false, true); (true, false); (true, true)]
        |> List.map (fun (input1: bool, input2: bool) -> (input1, input2, eval2 expression input1 input2))

    //48
    let table (variables: string list) (expression: bool_expr): ((string * bool) list * bool) list =
        let rec createTable (n: int): bool list list =
            let generate (rows: bool list list): bool list list =
                []
                |> List.foldBack (fun (element: bool list) (accumulator: bool list list) -> (true::element)::accumulator) rows
                |> List.foldBack (fun (element: bool list) (accumulator: bool list list) -> (false::element)::accumulator) rows
            match n with
            | 1 -> [[false]; [true]]
            | n when n > 1 -> n - 1 |> createTable |> generate
            | _ -> []
        let rec eval (expression: bool_expr) (values: bool list) =
            let substitute (variable: string) =
                values.[List.findIndex(fun (element: string) -> element = variable) variables]
            match expression, values with
            | Var(variable: string), values -> substitute variable
            | Not(inner: bool_expr), values -> not (eval inner values)
            | And(inner1: bool_expr, inner2: bool_expr), values -> (eval inner1 values) && (eval inner2 values)
            | Or(inner1: bool_expr, inner2: bool_expr), values -> (eval inner1 values) || (eval inner2 values)
        variables.Length
        |> createTable
        |> List.map (fun (inputs: bool list) -> (List.map2(fun (variable: string) (input: bool) -> (variable, input)) variables inputs, eval expression inputs))

    //49
    let rec gray (n: int): string list =
        let generate (codes: string list) =
            codes
            |> List.fold (fun (accumulator: string list) (element: string) -> ("1" + element)::accumulator) []
            |> List.foldBack (fun (element: string) (accumulator: string list) -> ("0" + element)::accumulator) codes
        match n with
        | 1 -> ["0"; "1"]
        | n when n > 1 -> n - 1 |> gray |> generate
        | _ -> []

    //50
    let huffman (frequencies: ('a * int) list): ('a * string) list =
        let get_count (tree: 'a huffman_tree): int =
            match tree with
            | Leaf(_, count: int) -> count
            | Internal((count: int), _, _) -> count
        let rec insert_in_order (treeList: 'a huffman_tree list) (tree: 'a huffman_tree): 'a huffman_tree list =
            match treeList with
            | [] -> tree::[]
            | (head: 'a huffman_tree)::(tail: 'a huffman_tree list) ->
                let treeCount: int = get_count tree
                let headCount: int = get_count head
                match treeCount > headCount with
                | true -> insert_in_order tail tree
                | false -> tree::treeList
        let rec tree_builder (treeList: 'a huffman_tree list): 'a huffman_tree list =
            match treeList with
            | (head1: 'a huffman_tree)::(head2: 'a huffman_tree)::(tail: 'a huffman_tree list) ->
                Internal(get_count head1 + get_count head2, head1, head2)
                |> insert_in_order tail
                |> tree_builder
            | _ -> treeList
        let build_dictionary (tree: 'a huffman_tree): ('a * string) list =
            let rec traverse (prefix: string) (tree: 'a huffman_tree) (dictionary: ('a * string) list): ('a * string) list =
                match tree with
                | Leaf(item: 'a, _) -> (item, prefix)::[]
                | Internal(_, left: 'a huffman_tree, right: 'a huffman_tree) ->
                    dictionary
                    |> traverse (prefix + "1") right
                    |> traverse (prefix + "0") left
            traverse "" tree []
        frequencies
        |> List.map (fun (item: 'a, count: int) -> Leaf((item, count)))
        |> List.sortByDescending get_count
        |> tree_builder
        |> List.head
        |> build_dictionary

