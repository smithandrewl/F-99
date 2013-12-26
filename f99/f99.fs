module f99

let rec my_last input = 
    match input with 
    | []           -> None
    | [head]       -> Some(head)
    | head :: tail -> my_last tail
    
let rec next_to_last input =
    match input with
    | []             -> None
    | [head]         -> None
    | head :: [last] -> Some(head)
    | head :: tail   -> next_to_last tail