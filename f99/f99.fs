module f99

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
    rev input = input

 // P07 (**) Flatten a nested list structure. 
 // F# does not support lists of multiple types

// P08 (**) Eliminate consecutive duplicates of list elements.
let remove_duplicates input =
    let rec iter input acc: 'a list = 
        match input with
        | [] -> rev acc
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
                 
    rev (pack_iter (input, [], []))