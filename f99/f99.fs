module f99

let rec my_last input = 
    match input with 
    | []           -> None
    | [head]       -> Some(head)
    | head :: tail -> my_last tail