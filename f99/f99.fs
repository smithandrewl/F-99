module f99

// F01 (*) Find the last element of a list.
let rec my_last input = 
    match input with 
    | []           -> None
    | [head]       -> Some(head)
    | head :: tail -> my_last tail

// F02 (*) Find the last but one element of a list.
let rec next_to_last input =
    match input with
    | []             -> None
    | [head]         -> None
    | head :: [last] -> Some(head)
    | head :: tail   -> next_to_last tail

// F03 (*) Find the K'th element of a list.
let rec element_at input n =
    match (input, n) with
    | ([], _)        -> None
    | (head :: _, 0) -> Some(head)
    | (_ :: tail, n) -> element_at tail (n - 1)