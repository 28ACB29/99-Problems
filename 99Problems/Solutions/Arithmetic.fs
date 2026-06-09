namespace Solutions

open System

module Arithmetic =

    /// <summary>Calculates the integer ceiling of the square root of <c>n</c>.</summary>
    /// <param name="n">Input integer.</param>
    /// <returns>Ceiling of the square root as int.</returns>
    let private highest (n: int): int =
        n
        |> float
        |> Math.Sqrt
        |> Math.Ceiling
        |> int

    /// <summary>Determines if a number is prime.</summary>
    /// <param name="n">Number to test.</param>
    /// <returns><c>true</c> when <c>n</c> is prime; otherwise <c>false</c>.</returns>
    let is_prime (n: int): bool =
        seq {2 .. highest n}
        |> Seq.exists (fun (element: int) -> n % element = 0)
        |> not

    /// <summary>Computes the greatest common divisor (GCD) of two integers.</summary>
    /// <param name="a">First integer.</param>
    /// <param name="b">Second integer.</param>
    /// <returns>The GCD of <c>a</c> and <c>b</c>.</returns>
    let rec gcd (a: int) (b: int): int =
        match a, b with
        | 0, b -> b
        | a, 0 -> a
        | 1, b -> 1
        | a, 1 -> 1
        | a, b when a = b -> a
        | a, b when a > b -> gcd (a % b) b
        | a, b when a < b -> gcd a (b % a)

    /// <summary>Checks whether two integers are coprime.</summary>
    /// <param name="a">First integer.</param>
    /// <param name="b">Second integer.</param>
    /// <returns><c>true</c> if GCD is 1; otherwise <c>false</c>.</returns>
    let coprime (a: int) (b: int): bool =
        (gcd a b) = 1

    /// <summary>Computes Euler's totient function φ(n) by counting factors (simple method).</summary>
    /// <param name="n">Input integer.</param>
    /// <returns>φ(n) approximate count using trial factors.</returns>
    let phi (n: int): int =
        seq {2 .. highest n}
        |> Seq.fold (fun (accumulator: int) (element: int) -> if n % element = 0 then accumulator + 1 else accumulator) 0

    /// <summary>Returns the list of distinct prime factors of <c>n</c>.</summary>
    /// <param name="n">Input integer.</param>
    /// <returns>List of distinct factors.</returns>
    let distinctFactors (n: int): int list =
        List.filter (fun (element: int) -> n % element = 0) [2 .. highest n]

    /// <summary>Decomposes <c>n</c> into its prime factors and counts occurrences.</summary>
    /// <param name="n">Input integer.</param>
    /// <returns>List of tuples (count, factor).</returns>
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

    /// <summary>Improved Euler's totient using prime factorization.</summary>
    /// <param name="n">Input integer.</param>
    /// <returns>φ(n) using the factorization formula.</returns>
    let phi_improved (n: int): int =
        n
        |> factors
        |> List.fold (fun (accumulator: int) ((count: int), (factor: int)) -> accumulator * (factor - 1) * (pown factor (count - 1))) 1

    /// <summary>Measures execution time (milliseconds) of a unary function applied to a parameter.</summary>
    /// <param name="unaryFunction">Function to time.</param>
    /// <param name="parameter">Parameter to pass to the function.</param>
    /// <returns>Elapsed time in milliseconds.</returns>
    let timeit (unaryFunction: 'a -> 'b) (parameter: 'a): float =
        let startDate: DateTime = DateTime.Now
        parameter
        |> unaryFunction
        |> ignore
        let endDate: DateTime = DateTime.Now
        (endDate - startDate).TotalMilliseconds

    /// <summary>Lists all primes in a closed interval [lower, upper].</summary>
    /// <param name="lower">Lower bound.</param>
    /// <param name="upper">Upper bound.</param>
    /// <returns>List of primes.</returns>
    let all_primes (lower: int) (upper: int): int list =
        [lower .. upper]
        |> List.filter(fun (element: int) -> is_prime element)

    /// <summary>Returns one Goldbach decomposition (p1, p2) for an even number.</summary>
    /// <param name="n">Even integer greater than 2.</param>
    /// <returns>Tuple of two primes summing to <c>n</c>, or (0,0) if not found.</returns>
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

    /// <summary>Computes Goldbach decompositions for a list of even numbers in range.</summary>
    /// <param name="lower">Lower bound of range.</param>
    /// <param name="upper">Upper bound of range.</param>
    /// <returns>List of tuples (n, (p1,p2)).</returns>
    let goldbach_list (lower: int) (upper: int): (int * (int * int)) list =
        all_primes (lower + (lower % 2)) (upper - (upper % 2))
        |> List.map (fun (element: int) -> (element, goldbach element))

    /// <summary>Filters Goldbach decompositions by a limit on the primes used.</summary>
    /// <param name="lower">Lower bound.</param>
    /// <param name="upper">Upper bound.</param>
    /// <param name="limit">Maximum allowed prime in decompositions.</param>
    /// <returns>Filtered list of Goldbach decompositions.</returns>
    let goldbach_limit (lower: int) (upper: int) (limit: int): (int * (int * int)) list =
        goldbach_list lower upper
        |> List.filter(fun (number: int, (element1: int, element2: int)) -> element1 <= limit && element2 <= limit)