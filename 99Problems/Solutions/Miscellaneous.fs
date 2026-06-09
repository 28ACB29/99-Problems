namespace Solutions

open System

module Miscellaneous =

    /// <summary>Generates all board positions for n x n chessboard.</summary>
    let queens_positions (n:int): (int * int) list =
        let rec generate_positions (row:int) (col:int) (acc:(int * int) list): (int * int) list =
            match row with
            | r when r >= n -> acc
            | _ ->
                match col with
                | c when c >= n -> generate_positions (row + 1) 0 acc
                | _ -> generate_positions row (col + 1) ((row, col) :: acc)
        generate_positions 0 0 []
        |> List.rev

    /// <summary>Converts a number into its word representation joined by hyphens (per-digit).</summary>
    let full_words (number:int):string =
        let match_digit (digit:int):string =
            match digit with
            | 0 -> "zero"
            | 1 -> "one"
            | 2 -> "two"
            | 3 -> "three"
            | 4 -> "four"
            | 5 -> "five"
            | 6 -> "six"
            | 7 -> "seven"
            | 8 -> "eight"
            | 9 -> "nine"
            | _ -> ""
        let rec extract_digits (stack:string list) (r:int):string list =
            match r with
            | 0 -> stack
            | _ -> extract_digits ((match_digit (r % 10))::stack) (r / 10)
        n
        |> extract_digits []
        |> String.Join "-"

    /// <summary>Checks whether a token is a valid identifier (letter followed by letters/digits/hyphens per implementation).</summary>
    let identifer (token:string):bool =
        let match_letter_or_digit (index:int) (token:string):bool =
            match index < token.Length with
            | true ->
                match token.[index] with
                | '-' -> 
                | _ -> false
            | false -> true
        let rec match_hyphen (index:int) (token:string):bool =
            match index < token.Length with
            | true ->
                match token.[index] with
                | '-' -> match_letter_or_digit (index + 1) (token)
                | _ -> false
            | false -> true
        match token with
        | "" -> false
        | _ -> 
            match Char.IsLetter token.[0] with
            | false -> false
            | true -> match_hyphen (1) (token)

    /// <summary>Lazy stream type.</summary>
    type 'a stream =
        | Nil
        | Cons of 'a * (unit -> 'a stream)

    /// <summary>Head of a stream; throws on empty.</summary>
    let hd (s:'a stream): 'a =
        match s with
        | Nil -> failwith "Empty stream"
        | Cons (h, _) -> h

    /// <summary>Tail of a stream; throws on empty.</summary>
    let tl (s:'a stream): 'a stream =
        match s with
        | Nil -> failwith "Empty stream"
        | Cons (_, t) -> t

    /// <summary>Takes first n elements from a stream and returns a list.</summary>
    let take (n:int) (s:'a stream): 'a list =
        let rec take_helper (count:int) (stream:'a stream) (acc:'a list): 'a list =
            match count with
            | c when c <= 0 -> List.rev acc
            | _ ->
                match stream with
                | Nil -> List.rev acc
                | Cons (h, t) -> take_helper (count - 1) (t()) (h :: acc)
        take_helper n s []

    /// <summary>Builds a stream by unfolding a state with function f.</summary>
    let unfold (f:'a -> ('b * 'a) option) (seed:'a): 'b stream =
        let rec unfold_helper (s:'a): 'b stream =
            match f s with
            | None -> Nil
            | Some (value, newState) -> Cons (value, fun () -> unfold_helper newState)
        unfold_helper seed

    /// <summary>Creates an infinite constant stream of the given value.</summary>
    let bang (x:'a): 'a stream =
        Cons (x, fun () -> bang x)

    /// <summary>Generates an infinite stream of increasing integers starting at x.</summary>
    let ints (x:int): int stream =
        let rec ints_helper (n:int): int stream =
            Cons (n, fun () -> ints_helper (n + 1))
        ints_helper x

    /// <summary>Maps a function over a stream.</summary>
    let map (f:'a -> 'b) (s:'a stream): 'b stream =
        let rec map_helper (stream:'a stream): 'b stream =
            match stream with
            | Nil -> Nil
            | Cons (h, t) -> Cons (f h, fun () -> map_helper (t()))
        map_helper s

    /// <summary>Filters a stream using a predicate.</summary>
    let filter (predicate:'a -> bool) (s:'a stream): 'a stream =
        let rec filter_helper (stream:'a stream): 'a stream =
            match stream with
            | Nil -> Nil
            | Cons (h, t) ->
                if predicate h then
                    Cons (h, fun () -> filter_helper (t()))
                else
                    filter_helper (t())
        filter_helper s

    /// <summary>Iterates side-effecting action over a stream (consumes stream).</summary>
    let iter (action:'a -> unit) (s:'a stream): unit =
        let rec iter_helper (stream:'a stream): unit =
            match stream with
            | Nil -> ()
            | Cons (h, t) ->
                action h
                iter_helper (t())
        iter_helper s

    /// <summary>Converts a lazy stream to a sequence.</summary>
    let to_seq (s:'a stream): seq<'a> =
        let rec to_seq_helper (stream:'a stream): seq<'a> =
            seq {
                match stream with
                | Nil -> ()
                | Cons (h, t) ->
                    yield h
                    yield! to_seq_helper (t())
            }
        to_seq_helper s

    /// <summary>Creates a stream from a sequence.</summary>
    let of_seq (s:seq<'a>): 'a stream =
        let enumerator = s.GetEnumerator()
        let rec of_seq_helper (): 'a stream =
            if enumerator.MoveNext() then
                Cons (enumerator.Current, fun () -> of_seq_helper ())
            else
                Nil
        of_seq_helper ()

    /// <summary>Extracts the main diagonal of a matrix represented as seq of seq.</summary>
    let diag (matrix:'a seq seq): 'a seq =
        let rec diag_helper (m:'a list list) (row:int) (col:int) (acc:'a list): 'a list =
            match row < List.length m && col < List.length (List.head m) with
            | true -> diag_helper m (row + 1) (col + 1) ((List.item col (List.item row m)) :: acc)
            | false -> List.rev acc
        diag_helper matrix 0 0 []