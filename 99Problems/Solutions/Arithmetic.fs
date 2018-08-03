namespace Solutions

open System

module Arithmetic =

    let private highest (n: int): int =
        n
        |> float
        |> Math.Sqrt
        |> Math.Ceiling
        |> int

    //31
    let is_prime (n: int): bool =
        seq {2 .. highest n}
        |> Seq.exists (fun (element: int) -> n % element = 0)
        |> not

    //32
    let rec gcd (a: int) (b: int): int =
        match a, b with
        | 0, b -> b
        | a, 0 -> a
        | 1, b -> 1
        | a, 1 -> 1
        | a, b when a = b -> a
        | a, b when a > b -> gcd (a % b) b
        | a, b when a < b -> gcd a (b % a)

    //33
    let coprime (a: int) (b: int): bool =
        (gcd a b) = 1

    //34
    let phi (n: int): int =
        seq {2 .. highest n}
        |> Seq.fold (fun (accumulator: int) (element: int) -> if n % element = 0 then accumulator + 1 else accumulator) 0

    //35
    let distinctFactors (n: int): int list =
        List.filter(fun (element: int) -> n % element = 0) [2 .. highest n]

    //36
    let factors (n: int): (int * int) list =
        let rec getFactors(i: int): int list =
            let highest: int =
                i
                |> float
                |> Math.Sqrt
                |> Math.Ceiling
                |> int
            let rec getFactor (j: int): int option =
                match i % j = 0 with
                | true -> Some j
                | false ->
                    match j = highest with
                    | true -> None
                    | false -> getFactor (j + 1)
            let factor = getFactor 2
            match factor with
            | None -> i::[]
            | Some factor -> factor::(getFactors (i / factor))
        n
        |> getFactors
        |> WorkingWithLists.encode

    //37
    let phi_improved (n: int): int =
        n
        |> factors
        |> List.fold (fun (accumulator: int) ((count: int), (factor: int)) -> accumulator * (factor - 1) * (pown factor (count - 1))) 1

    //38
    let timeit (unaryFunction: 'a -> 'b) (parameter: 'a): float =
        let startDate: DateTime = DateTime.Now
        parameter
        |> unaryFunction
        |> ignore
        let endDate: DateTime = DateTime.Now
        (endDate - startDate).TotalMilliseconds

    //39
    let all_primes (lower: int) (upper: int): int list =
        [lower .. upper]
        |> List.filter(fun (element: int) -> is_prime element)

    //40
    let goldbach (n: int): (int * int) =
        let rec tailCall (candidates: int list): (int * int) =
            match candidates with
            | (head: int)::[] when n - head |> is_prime -> (head, n - head)
            | (head: int)::(tail: int list) when n - head |> is_prime -> (head, n - head)
            | (head: int)::(tail: int list) when n - head |> is_prime |> not -> tailCall tail
            | _ ->(0, 0)
        n
        |> all_primes 2
        |> tailCall

    //41
    let goldbach_list (lower: int) (upper: int): (int * (int * int)) list =
        all_primes (lower + (lower % 2)) (upper - (upper % 2))
        |> List.map (fun (element: int) -> (element, goldbach element))

    //41
    let goldbach_limit (lower: int) (upper: int) (limit: int): (int * (int * int)) list =
        goldbach_list lower upper
        |> List.filter(fun (number: int, (element1: int, element2: int)) -> element1 <= limit && element2 <= limit)