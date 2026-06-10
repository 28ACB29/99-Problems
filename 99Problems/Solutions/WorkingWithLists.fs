namespace Solutions

open System

module WorkingWithLists =

    /// <summary>Type for nested list nodes used by the flatten function.</summary>
    type 'a node =
    | One of 'a 
    | Many of 'a node list

    /// <summary>Run-length encoding element (single or many).</summary>
    type 'a rle =
    | One of 'a
    | Many of int * 'a

    /// <summary>Modulo that handles negative values as positive wrap-around.</summary>
    /// <param name="n">Value to mod.</param>
    /// <param name="m">Modulus.</param>
    /// <returns>Positive remainder in range [0, m-1].</returns>
    let modulo (n: int) (m: int) =
        match Math.Sign(n) with
        | -1 -> (m - ((-n) % m))
        | 0 -> 0
        | 1 -> n % m

    /// <summary>Skips the first <c>index</c> elements of a list.</summary>
    /// <param name="index">Number of elements to skip (1-based handling in implementation).</param>
    /// <param name="genericList">Input list.</param>
    /// <returns>Remaining list after skipping.</returns>
    let rec skip (index: int) (genericList: 'a list): 'a list =
        match index, genericList with
        | 1, (head: 'a)::(tail: 'a list) -> tail
        | index, (head: 'a)::(tail: 'a list) when index > 1 -> skip (index - 1) tail
        | _ -> []

    /// <summary>Takes the first <c>index</c> elements of a list.</summary>
    /// <param name="index">Number of elements to take.</param>
    /// <param name="genericList">Input list.</param>
    /// <returns>List containing the first <c>index</c> elements.</returns>
    let rec take (index: int) (genericList: 'a list): 'a list =
        match index, genericList with
        | 1, (head: 'a)::(tail: 'a list) -> head::[]
        | index, (head: 'a)::(tail: 'a list) when index > 1 -> head::(take (index - 1) tail)
        | _ -> []

    /// <summary>Returns the last element of a list, if any.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>Option of last element.</returns>
    let rec last (genericList: 'a list): 'a option =
        match genericList with
        | [] -> None
        | (head: 'a)::[] -> Some head
        | (head: 'a)::(tail: 'a list) -> last tail

    /// <summary>Returns the last two elements of a list, if available.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>Option containing last two elements as a list.</returns>
    let rec last_two (genericList: 'a list): 'a list option =
        match genericList with
        | [] -> None
        | (head: 'a)::[] -> None
        | (head1: 'a)::(head2: 'a)::[] -> Some (head1::head2::[])
        | (head1: 'a)::(head2: 'a)::(tail: 'a list) -> last_two (head2::tail)

    /// <summary>Returns the element at 1-based index, if present.</summary>
    /// <param name="index">1-based index.</param>
    /// <param name="genericList">Input list.</param>
    /// <returns>Option of the element at index.</returns>
    let rec at (index: int) (genericList: 'a list): 'a option =
        match index, genericList with
        | index, (head: 'a)::(tail: 'a list) when index = 1 -> Some head
        | index, (head: 'a)::(tail: 'a list) when index > 1 -> at (index - 1) tail
        | _ -> None

    /// <summary>Computes the length of a list (tail-recursive).</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>Length of the list.</returns>
    let length (genericList: 'a list): int =
        let rec tailCall (length: int) (genericList: 'a list): int =
            match genericList with
            | [] -> length
            | (_)::(tail: 'a list) -> tailCall (length + 1) tail
        tailCall 0 genericList

    /// <summary>Reverses a list (tail-recursive).</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>Reversed list.</returns>
    let rev (genericList: 'a list): 'a list =
        let rec tailCall (reversed: 'a list) (genericList: 'a list): 'a list =
            match genericList with
            | [] -> reversed
            | (head: 'a)::(tail: 'a list) -> tailCall (head::reversed) tail
        tailCall [] genericList

    /// <summary>Checks whether a list is a palindrome.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns><c>true</c> if palindrome; otherwise <c>false</c>.</returns>
    let is_palindrome (genericList: 'a list): bool =
        genericList = List.rev genericList

    /// <summary>Flattens a nested node structure into a flat list.</summary>
    /// <param name="nestedList">Nested node value.</param>
    /// <returns>Flattened list of values.</returns>
    let rec flatten (nestedList: 'a node): 'a list =
        match nestedList with
        | node.One one -> one::[]
        | node.Many many -> List.foldBack(fun (element: 'a node) (accumulator: 'a list) -> (flatten element)@accumulator) many []

    /// <summary>Removes consecutive duplicates from a list.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>List with consecutive duplicates removed.</returns>
    let compress (genericList: 'a list): 'a list =
        let compressRight (element: 'a) (accumulator: 'a list): 'a list =
            match element, accumulator with
            | element, (current: 'a)::(next: 'a list) when element = current -> accumulator
            | _ -> element::accumulator
        List.foldBack compressRight genericList []

    /// <summary>Packs consecutive duplicates into sublists.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>List of packed sublists.</returns>
    let pack (genericList: 'a list): 'a list list =
        let packRight (element: 'a) (accumulator: 'a list list): 'a list list =
            match element, accumulator with
            | element, (current: 'a list)::(next: 'a list list) when element = current.Head -> (element::current)::next
            | _ -> (element::[])::accumulator
        List.foldBack packRight genericList []

    /// <summary>Run-length encodes a list as (count,item) pairs.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>List of (count, item) pairs.</returns>
    let encode (genericList: 'a list): (int * 'a) list =
        let encodeRight (element: 'a) (accumulator: (int * 'a) list): (int * 'a) list =
            match element, accumulator with
            | element, (count: int, item: 'a)::(next: (int * 'a) list) when element = item -> (count + 1, element)::next
            | _ -> (1, element)::accumulator
        List.foldBack encodeRight genericList []

    /// <summary>Run-length encodes a list using RLE discriminated union representation.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>RLE encoded list.</returns>
    let encode_rle (genericList: 'a list): 'a rle list =
        let encodeRight (element: 'a) (accumulator: 'a rle list): 'a rle list =
            match element, accumulator with
            | element, rle.One(item: 'a)::(next: 'a rle list) when element = item -> rle.Many(2, element)::next
            | element, rle.Many(count: int, item: 'a)::(next: 'a rle list) when element = item -> rle.Many(count + 1, element)::next
            | _ -> rle.One(element)::accumulator
        List.foldBack encodeRight genericList []

    /// <summary>Decodes a list encoded as (count,item) pairs.</summary>
    /// <param name="encoded">Encoded (count,item) list.</param>
    /// <returns>Decoded flat list.</returns>
    let decode_list (encoded: (int * 'a) list): 'a list =
        encoded
        |> List.collect (fun (count: int, item: 'a) -> List.init count (fun _ -> item))

    /// <summary>Decodes a run-length encoded list represented with <c>'a rle</c>.</summary>
    /// <param name="encoded">RLE encoded list.</param>
    /// <returns>Decoded flat list.</returns>
    let rec decode (encoded: 'a rle list): 'a list =
        let rec repeat (genericElement: 'a) (n: int): 'a list =
            match n with
            | 0 -> []
            | _ -> genericElement::(repeat genericElement (n - 1))
        match encoded with
        | [] -> []
        | rle.One(item: 'a)::(tail: 'a rle list) -> item::(decode tail)
        | rle.Many(count: int, item: 'a)::(tail: 'a rle list) -> (repeat item count)@(decode tail)

    /// <summary>Duplicates each element in the list.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>List where each element appears twice in sequence.</returns>
    let rec duplicate (genericList: 'a list): 'a list =
        List.foldBack (fun (element: 'a) (accumulator: 'a list) -> element::element::accumulator) genericList []

    /// <summary>Replicates each element <c>n</c> times.</summary>
    /// <param name="genericList">Input list.</param>
    /// <param name="n">Replication count for each element.</param>
    /// <returns>List with each element replicated <c>n</c> times.</returns>
    let replicate (genericList: 'a list) (n: int): 'a list =
        let rec repeat (genericElement: 'a) (n: int) (accumulator: 'a list): 'a list =
            match n with
            | 0 -> accumulator
            | _ -> repeat genericElement (n - 1) (genericElement::accumulator)
        List.foldBack (fun (element: 'a) (accumulator: 'a list) -> repeat element n accumulator) genericList []

    /// <summary>Removes every <c>multiple</c>-th element from the list.</summary>
    /// <param name="genericList">Input list.</param>
    /// <param name="multiple">Step multiple for removals.</param>
    /// <returns>List with every multiple-th element removed.</returns>
    let drop (genericList: 'a list) (multiple: int): 'a list =
        let last = multiple - 1
        let rec tailCall (i: int) (genericList: 'a list): 'a list =
            match genericList, i with
                | (head: 'a)::(tail: 'a list), i when i % multiple = last -> tailCall (i - 1) tail
                | (head: 'a)::(tail: 'a list), i when i % multiple <> last -> head::(tailCall (i - 1) tail)
                | _ -> genericList
        tailCall 0 genericList

    /// <summary>Splits a list at the given index into a pair of lists.</summary>
    /// <param name="genericList">Input list.</param>
    /// <param name="index">Split index.</param>
    /// <returns>Tuple of (firstPart, secondPart).</returns>
    let split (genericList: 'a list) (index: int): ('a list * 'a list) =
        let rec split_internal (differenceList: 'a list -> 'a list, rest: 'a list) (index: int): ('a list * 'a list) =
            match index with
            | 0 -> (differenceList [], rest)
            | _ ->
                
                match rest with
                | [] -> (differenceList [], rest)
                | (head: 'a)::(tail: 'a list) -> 
                    split_internal (differenceList << List.Cons head, tail) (index - 1)
        match index, genericList with
        | index, genericList when index < 0 -> ([], genericList)
        | index, genericList when index >= genericList.Length -> (genericList, [])
        | _ -> split_internal (id, genericList) (index)

    /// <summary>Extracts a slice from index <c>i</c> to <c>k</c> (inclusive, 1-based indices).</summary>
    /// <param name="genericList">Input list.</param>
    /// <param name="i">Start index (1-based).</param>
    /// <param name="k">End index (1-based).</param>
    /// <returns>Sliced list.</returns>
    let rec slice (genericList: 'a list) (i: int) (k: int): 'a list =
        genericList
        |> skip i
        |> take (k - i + 1)

    /// <summary>Rotates a list left by <c>index</c> positions (supports negative via modulo).</summary>
    /// <param name="genericList">Input list.</param>
    /// <param name="index">Number of positions to rotate.</param>
    /// <returns>Rotated list.</returns>
    let rotate (genericList: 'a list) (index: int): ('a list) =
        match index, genericList with
        | index, genericList when index = 0 -> genericList
        | _ -> (skip (modulo index (genericList.Length)) genericList)@(take (modulo index (genericList.Length)) genericList)

    /// <summary>Removes the element at index (0-based modulo list length).</summary>
    /// <param name="index">Index to remove (wraps using modulo).</param>
    /// <param name="genericList">Input list.</param>
    /// <returns>List with the element removed.</returns>
    let rec remove_at (index: int) (genericList: 'a list): 'a list =
        let constrainedIndex: int = modulo index genericList.Length
        match constrainedIndex, genericList with
            | 0, (head: 'a)::(tail: 'a list) -> tail
            | constrainedIndex, (head: 'a)::(tail: 'a list) when constrainedIndex = index -> head::(remove_at (constrainedIndex - 1) tail)
            | _ -> genericList

    /// <summary>Inserts an item at a given index (clamped to valid range).</summary>
    /// <param name="item">Item to insert.</param>
    /// <param name="index">Index at which to insert.</param>
    /// <param name="genericList">Input list.</param>
    /// <returns>List with item inserted.</returns>
    let rec insert_at (item: 'a) (index: int) (genericList: 'a list): 'a list =
        let constrainedIndex: int = Math.Min(Math.Max(index, 0), genericList.Length)
        match constrainedIndex, genericList with
            | 0, genericList -> item::genericList
            | constrainedIndex, (head: 'a)::(tail: 'a list) when constrainedIndex > 0 -> head::(insert_at item (constrainedIndex - 1) tail)
            | _ -> genericList

    /// <summary>Generates a range of integers from start to stop (inclusive).</summary>
    /// <param name="start">Start value.</param>
    /// <param name="stop">Stop value.</param>
    /// <returns>List of integers from start to stop.</returns>
    let range (start: int) (stop: int): int list =
        let step: int = stop.CompareTo(start)
        let rec tailCall (accumulator: int list) (position: int): int list =
            match position with
            | position when position <> start -> tailCall (position::accumulator) (position - step)
            | position when position = start -> position::accumulator
            | _ -> []
        tailCall [] stop

    /// <summary>Selects <c>taken</c> random elements from a list (without replacement).</summary>
    /// <param name="genericList">Input list.</param>
    /// <param name="taken">Number of elements to select.</param>
    /// <returns>List of randomly selected elements.</returns>
    let rec rand_select (genericList: 'a list) (taken: int): 'a list =
        let index = (Random()).Next(genericList.Length)
        match taken with
        | 0 -> []
        | _ -> genericList.[index]::(rand_select (remove_at index genericList) (taken - 1))

    /// <summary>Selects <c>taken</c> random numbers between 1 and <c>maximum</c>.</summary>
    /// <param name="taken">Count to select.</param>
    /// <param name="maximum">Maximum value.</param>
    /// <returns>List of selected numbers.</returns>
    let rec lotto_select (taken: int) (maximum: int): int list =
        rand_select [1 .. maximum] taken

    /// <summary>Generates a random permutation of the list.</summary>
    /// <param name="genericList">Input list.</param>
    /// <returns>Random permutation of the input list.</returns>
    let rec permutation (genericList: 'a list): 'a list =
        rand_select genericList genericList.Length

    /// <summary>Extracts combinations of size <c>n</c> from the list.</summary>
    /// <param name="genericList">Input list.</param>
    /// <param name="n">Combination size.</param>
    /// <returns>List of combinations (each combination is a list).</returns>
    let rec extract (genericList: 'a list) (n:int): 'a list list =
        match n, genericList with
        | 0, _ -> [[]]
        | _, [] -> []
        | n, head::tail ->
            let withHead = extract tail (n - 1) |> List.map (fun sublist -> head::sublist)
            let withoutHead = extract tail n
            withHead @ withoutHead
            n
            |> extract tail
            |> List.foldBack (fun (sublist:'a list, acc:'a list list) -> (head::sublist)::acc) extract tail (n - 1)

    /// <summary>Groups a list according to given sizes (uses extract internally).</summary>
    /// <param name="genericList">Input list.</param>
    /// <param name="sizes">List of group sizes.</param>
    /// <returns>List of grouped partitions.</returns>
    let group (genericList: 'a list) (sizes: int list): 'a list list =
        let splitIntoChunks (sizes: int list) (genericList: 'a list): 'a list list =
            let rec tailCall (accumulator: 'a list list) (remaining: 'a list) (sizes: int list): 'a list list =
                match remaining, sizes with
                | _, [] -> accumulator @ [remaining]
                | [], _ -> accumulator
                | head::tail, size::sizesTail ->
                    let (chunk, rest) = split remaining size
                    tailCall (accumulator @ [chunk]) rest sizesTail
            tailCall [] genericList sizes
        let n:int = sizes.Length
        genericList
        |> extract n
        |> splitIntoChunks sizes 

    /// <summary>Sorts a list of lists by length (ascending).</summary>
    /// <param name="genericListOfLists">List of lists to sort.</param>
    /// <returns>Sorted list of lists by length.</returns>
    let length_sort (genericListOfLists: 'a list list): 'a list list =
        genericListOfLists
        |> List.sortBy (fun (element: 'a list) -> element.Length)

    /// <summary>Sorts by frequency of list lengths (intended helper; original implementation may contain a typo).</summary>
    /// <param name="genericListOfLists">List of lists to frequency-sort.</param>
    /// <returns>Frequency-sorted list of lists.</returns>
    let frequency_sort (genericListOfLists: 'a list list): 'a list list =
        genericListOfLists
        |> List.groupby (fun (genericList: 'a list) -> genericList.Length)
        |> List.sortby (fun (length:int, _) -> length)
        |> List.collect (fun (_, groupedLists: 'a list list) -> grouepdLists)
