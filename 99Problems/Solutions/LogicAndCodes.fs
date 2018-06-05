namespace Solutions

module LogicAndCodes =

    type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr

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

