module f99

exception InvalidIndex    of string
exception IllegalArgument of string

// P01 (*) Find the last element of a list.
let rec my_last input = 
    match input with 
    | []           -> None
    | [head]       -> Some(head)
    | head :: tail -> my_last tail

// P02 (*) Find the last but one element of a list.
let rec next_to_last input =
    match input with
    | []             -> None
    | [head]         -> None
    | head :: [last] -> Some(head)
    | head :: tail   -> next_to_last tail

// P03 (*) Find the K'th element of a list.
let rec element_at input n =
    match (input, n) with
    | ([], _)        -> None
    | (head :: _, 0) -> Some(head)
    | (_ :: tail, n) -> element_at tail (n - 1)
    
// P04 (*) Find the number of elements of a list.
let num_of_elements input =
    let rec iter input count =
        match input with 
        | []        -> count
        | _ :: tail -> iter tail (count + 1)
    
    iter input 0
 
// P05 (*) Reverse a list.
let rev input =
    let rec iter input acc =
        match input with
        | []           -> acc
        | head :: tail -> iter tail (head :: acc)   
    
    iter input []
    
 // P06 (*) Find out whether a list is a palindrome.
let is_pal input =
    List.rev input = input

 // P07 (**) Flatten a nested list structure. 
 // F# does not support lists of multiple types

// P08 (**) Eliminate consecutive duplicates of list elements.
let remove_duplicates input =
    let rec iter input acc: 'a list = 
        match input with
        | [] -> List.rev acc
        | head :: tail ->
            if (acc.IsEmpty) || (head <> acc.Head) then
                iter tail (head :: acc)
            else
                iter tail acc
    
    iter input [] 

// P09 (**) Pack consecutive duplicates of list elements into sublists.
let pack input =

    let rec pack_iter (input, acc: 'a list list, sublist_acc: 'a list) =
        match (input, sublist_acc) with
        | ([], [])                                          -> acc
        | ([], sublist)                                     -> (sublist_acc :: acc)
        | (head :: tail, [])                                -> pack_iter(tail, acc, ([head]))
        | (head :: tail, sublist) when head <> sublist.Head -> pack_iter(tail, (sublist :: acc), [head])
        | (head :: tail, sublist)                           -> pack_iter(tail, acc, (head :: sublist))
                 
    List.rev (pack_iter (input, [], []))

// P10 (*) Run­length encoding of a list.
let encode (input: 'a list) =
    let grouped = pack input
    
    List.map (fun x -> (List.length x, List.head x)) grouped

// P12 (**) Decode a run­length encoded list.
let decode items =
    let expand item =
        let rec iter item acc =
            match item with
            | (0, _)        -> acc
            | (times, char) -> iter (times - 1, char) (char :: acc) 
        
        iter item []
    
    List.map (fun item -> expand item) items

// P13 (**) Run­length encoding of a list (direct solution)
let encodeDirect (input: 'a list) =
    let rec iter (input: 'a list, current: (int * 'a), acc: (int * 'a) list) =
       match (input, current) with
       | ([], current)                                -> current :: acc
       | (head :: tail, (count, chr)) when chr = head -> iter (tail, (count + 1, chr), acc) 
       | (head :: tail, current)                      -> iter (tail, (1, head), current :: acc) 
    
    match input with
    | []           -> []
    | head :: tail -> List.rev (iter (List.tail input, (1, List.head input), []))

// P14 (*) Duplicate the elements of a list.
let duplic input:'a list =
    let rec iter (input: 'a list, acc: 'a list) =
        match input with
        | []           -> acc
        | head :: tail -> iter(tail, head :: head :: acc)

    List.rev(iter (input, []))

// P15 (**) Duplicate the elements of a list a given number of times.
let dupli (input:'a list, times: int) =
    let rec duplicate (item: 'a, times: int, acc: 'a list) =
        match times with
        | 0 -> acc
        | _ -> duplicate (item, (times - 1), item :: acc)
    
    match input with
    | [] -> []
    | _  -> 
        List.map (fun item -> duplicate (item, times, [])) input
        |> List.reduce (@)
        
// P16 (**) Drop every N'th element from a list.
let drop (input: 'a list, n: int) =
    let rec iter (input, cur, acc) =
        match (input, cur) with
        | ([], _)            -> acc
        | (__::tail, 1)      -> iter(tail, n, acc)
        | (head::tail, cur)  -> iter (tail, cur - 1, head :: acc) 
    
    match n with
    | 0 -> []
    | 1 -> []
    | _ -> List.rev(iter (input, n, []))

// P17 (*) Split a list into two parts; the length of the first part is given
let split (input:'a list, splitAt: int) =
    let rec iter(input:'a list, depth, acc) =
        match input with 
        | []           -> (List.rev acc, input)
        | head :: tail -> 
            match depth = splitAt with
            | true  -> (List.rev acc, head :: tail)
            | false -> iter(tail, depth + 1, head :: acc)

    let len         = List.length input
    let outOfBounds = (splitAt < 0) || (splitAt > len)
    
    match outOfBounds with 
    | true  -> None
    | false -> Some (iter(input, 0, []))

// P18 (**) Extract a slice from a list.
let slice(lst:'a list, start: int, stop: int) =
    match (start, stop) with
    | (start, stop) when start > stop            -> raise(IllegalArgument("start must be less than or equal to stop"))
    | (_, stop)     when stop > (lst.Length - 1) -> raise(InvalidIndex("stop cannot be past the end of the list"))
    | (start, _)    when (start < 0)             -> raise(InvalidIndex("start must be greater or equal to zero"))
    | (_, stop)     when (stop < 0)              -> raise(InvalidIndex("stop must be greater or equal to zero"))
    | (stop, start) when stop = start            -> [(element_at lst (start)).Value]
    | (stop, start) ->
        let leftRemoved  = snd(split(lst, start).Value) in
        let rightRemoved = fst(split(leftRemoved, stop - start).Value) in
        rightRemoved

//P20 (*) Remove the K'th element from a list.
let remove_at (lst: 'a list, idx: int) =
    let rec iter(lst: 'a list, current:int, acc: 'a list) =
        match (lst, current = idx) with
        | ([], _)               -> acc
        | (_ :: tail, true)     -> iter(tail, current + 1, acc)
        | (head :: tail, false) -> iter(tail, current + 1, head :: acc)
    
    List.rev (iter(lst, 0, []))
    
//P21 (*) Insert an element at a given position into a list.
let insert_at(item: 'a, lst: 'a list, idx: int) =
    
    match (lst, idx) with
    | (_, idx) when idx < 0                  -> raise(InvalidIndex("idx was less than zero"))
    | (lst, idx) when idx > List.length(lst) -> raise(InvalidIndex("idx was greater than the length of the list"))
    | _                                      -> 
        match split(lst, idx) with
        | None -> lst
        | Some((left, right)) -> left @ [item] @ right
    
    
//P22 (*) Create a list containing all integers within a given range.
let range(start: int, stop: int) =
    let rec iter(curr: int, acc: int list) =
        if (curr > stop) then acc
        else iter(curr + 1, curr :: acc)
    
    match (start, stop) with
    | (start, stop) when start > stop -> None
    | (start, stop) when start = stop -> Some([stop])
    | (_, _)                          -> Some(List.rev (iter(start, [])))

// P23 (**) Extract a given number of randomly selected elements from a list.
let rnd_select(lst, num) =
    let rand = System.Random()
    let len = List.length lst

    let rec iter(lst, num) =
        match num with
        | 0 -> lst
        | _ -> iter(remove_at(lst, rand.Next(0, len)), num - 1)
 
    match (lst, num) with
    | (_, 0) -> []
    | (_, num) when num > (List.length lst) -> raise(InvalidIndex("The list does not contain at least num elements"))
    | _ -> iter(lst, num)
