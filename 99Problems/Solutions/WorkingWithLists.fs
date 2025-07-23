namespace Solutions

open System

module WorkingWithLists =

    type 'a node =
    | One of 'a 
    | Many of 'a node list

    type 'a rle =
    | One of 'a
    | Many of int * 'a

    let rec skip (index: int) (genericList: 'a list): 'a list =
        match index, genericList with
        | 1, (head: 'a)::(tail: 'a list) -> tail
        | index, (head: 'a)::(tail: 'a list) when index > 1 -> skip (index - 1) tail
        | _ -> []

    let rec take (index: int) (genericList: 'a list): 'a list =
        match index, genericList with
        | 1, (head: 'a)::(tail: 'a list) -> head::[]
        | index, (head: 'a)::(tail: 'a list) when index > 1 -> head::(take (index - 1) tail)
        | _ -> []

    let modulo (n: int) (m: int) =
        match Math.Sign(n) with
        | -1 -> (m - ((-n) % m))
        | 0 -> 0
        | 1 -> n % m

    //1
    let rec last (genericList: 'a list): 'a option =
        match genericList with
        | [] -> None
        | (head: 'a)::[] -> Some head
        | (head: 'a)::(tail: 'a list) -> last tail

    //2
    let rec last_two (genericList: 'a list): 'a list option =
        match genericList with
        | [] -> None
        | (head: 'a)::[] -> None
        | (head1: 'a)::(head2: 'a)::[] -> Some (head1::head2::[])
        | (head1: 'a)::(head2: 'a)::(tail: 'a list) -> last_two (head2::tail)

    //3
    let rec at (index: int) (genericList: 'a list): 'a option =
        match index, genericList with
        | index, (head: 'a)::(tail: 'a list) when index = 1 -> Some head
        | index, (head: 'a)::(tail: 'a list) when index > 1 -> at (index - 1) tail
        | _ -> None

    //4
    let length (genericList: 'a list): int =
        let rec tailCall (length: int) (genericList: 'a list): int =
            match genericList with
            | [] -> length
            | (_)::(tail: 'a list) -> tailCall (length + 1) tail
        tailCall 0 genericList

    //5
    let rev (genericList: 'a list): 'a list =
        let rec tailCall (reversed: 'a list) (genericList: 'a list): 'a list =
            match genericList with
            | [] -> reversed
            | (head: 'a)::(tail: 'a list) -> tailCall (head::reversed) tail
        tailCall [] genericList

    //6
    let is_palindrome (genericList: 'a list): bool =
        genericList = List.rev genericList

    //7
    let rec flatten (nestedList: 'a node): 'a list =
        match nestedList with
        | node.One one -> one::[]
        | node.Many many -> List.foldBack(fun (element: 'a node) (accumulator: 'a list) -> (flatten element)@accumulator) many []

    //8
    let compress (genericList: 'a list): 'a list =
        let compressRight (element: 'a) (accumulator: 'a list): 'a list =
            match element, accumulator with
            | element, (current: 'a)::(next: 'a list) when element = current -> accumulator
            | _ -> element::accumulator
        List.foldBack compressRight genericList []

    //9
    let pack (genericList: 'a list): 'a list list =
        let packRight (element: 'a) (accumulator: 'a list list): 'a list list =
            match element, accumulator with
            | element, (current: 'a list)::(next: 'a list list) when element = current.Head -> (element::current)::next
            | _ -> (element::[])::accumulator
        List.foldBack packRight genericList []

    //10
    let encode (genericList: 'a list): (int * 'a) list =
        let encodeRight (element: 'a) (accumulator: (int * 'a) list): (int * 'a) list =
            match element, accumulator with
            | element, (count: int, item: 'a)::(next: (int * 'a) list) when element = item -> (count + 1, element)::next
            | _ -> (1, element)::accumulator
        List.foldBack encodeRight genericList []

    //11
    let encode_rle (genericList: 'a list): 'a rle list =
        let encodeRight (element: 'a) (accumulator: 'a rle list): 'a rle list =
            match element, accumulator with
            | element, rle.One(item: 'a)::(next: 'a rle list) when element = item -> rle.Many(2, element)::next
            | element, rle.Many(count: int, item: 'a)::(next: 'a rle list) when element = item -> rle.Many(count + 1, element)::next
            | _ -> rle.One(element)::accumulator
        List.foldBack encodeRight genericList []

    let decode_list (encoded: (int * 'a) list): 'a list =
        encoded
        |> List.collect (fun (count: int, item: 'a) -> List.init count (fun _ -> item))

    //12
    let rec decode (encoded: 'a rle list): 'a list =
        let rec repeat (genericElement: 'a) (n: int): 'a list =
            match n with
            | 0 -> []
            | _ -> genericElement::(repeat genericElement (n - 1))
        match encoded with
        | [] -> []
        | rle.One(item: 'a)::(tail: 'a rle list) -> item::(decode tail)
        | rle.Many(count: int, item: 'a)::(tail: 'a rle list) -> (repeat item count)@(decode tail)

    //14
    let rec duplicate (genericList: 'a list): 'a list =
        match genericList with
        | [] -> []
        | (head: 'a)::(tail: 'a list) -> head::head::(duplicate tail)

    //15
    let replicate (genericList: 'a list) (n: int): 'a list =
        let rec repeat (genericElement: 'a) (n: int) (accumulator: 'a list): 'a list =
            match n with
            | 0 -> accumulator
            | _ -> repeat genericElement (n - 1) (genericElement::accumulator)
        List.foldBack (fun (element: 'a) (accumulator: 'a list) -> repeat element n accumulator) genericList []

    //16
    let drop (genericList: 'a list) (multiple: int): 'a list =
        let last = multiple - 1
        let rec tailCall (i: int) (genericList: 'a list): 'a list =
            match genericList, i with
                | (head: 'a)::(tail: 'a list), i when i % multiple = last -> tailCall (i - 1) tail
                | (head: 'a)::(tail: 'a list), i when i % multiple <> last -> head::(tailCall (i - 1) tail)
                | _ -> genericList
        tailCall 0 genericList

    //17
    let rec split (genericList: 'a list) (index: int): ('a list * 'a list) =
        match index, genericList with
        | index, genericList when index < 0 -> ([], genericList)
        | index, genericList when index >= genericList.Length -> (genericList, [])
        | _ -> (take index genericList, skip index genericList)

    //18
    let rec slice (genericList: 'a list) (i: int) (k: int): 'a list =
        genericList
        |> skip i
        |> take (k - i + 1)

    //19
    let rotate (genericList: 'a list) (index: int): ('a list) =
        match index, genericList with
        | index, genericList when index = 0 -> genericList
        | _ -> (skip (modulo index (genericList.Length)) genericList)@(take (modulo index (genericList.Length)) genericList)

    //20
    let rec remove_at (index: int) (genericList: 'a list): 'a list =
        let constrainedIndex = modulo index genericList.Length
        match constrainedIndex, genericList with
            | 0, (head: 'a)::(tail: 'a list) -> tail
            | constrainedIndex, (head: 'a)::(tail: 'a list) when constrainedIndex = index -> head::(remove_at (constrainedIndex - 1) tail)
            | _ -> genericList

    //21
    let rec insert_at (item: 'a) (index: int) (genericList: 'a list): 'a list =
        let constrainedIndex = Math.Min(Math.Max(index, 0), genericList.Length)
        match constrainedIndex, genericList with
            | 0, genericList -> item::genericList
            | constrainedIndex, (head: 'a)::(tail: 'a list) when constrainedIndex > 0 -> head::(insert_at item (constrainedIndex - 1) tail)
            | _ -> genericList

    //22
    let range (start: int) (stop: int): int list =
        let step: int = stop.CompareTo(start)
        let rec tailCall (accumulator: int list) (position: int): int list =
            match position with
            | position when position <> start -> tailCall (position::accumulator) (position - step)
            | position when position = start -> position::accumulator
            | _ -> []
        tailCall [] stop

    //23
    let rec rand_select (genericList: 'a list) (taken: int): 'a list =
        let index = (Random()).Next(genericList.Length)
        match taken with
        | 0 -> []
        | _ -> genericList.[index]::(rand_select (remove_at index genericList) (taken - 1))

    //24
    let rec lotto_select (taken: int) (maximum: int): int list =
        rand_select [1 .. maximum] taken

    //25
    let rec permutation (genericList: 'a list): 'a list =
        rand_select genericList genericList.Length

    //26

    //27

    //28
    let length_sort (genericListOfLists: 'a list list): 'a list list =
        genericListOfLists
        |> List.sortBy (fun (element: 'a list) -> element.Length)
