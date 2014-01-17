module f99

exception InvalidIndex of string

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
//Example:
//?­ range(4,9,L).
//L = [4,5,6,7,8,9]
let range(start: int, stop: int) =
    let rec iter(curr: int, acc: int list) =
        if (curr > stop) then acc
        else iter(curr + 1, curr :: acc)
    
    match (start, stop) with
    | (start, stop) when start > stop -> None
    | (start, stop) when start = stop -> Some([stop])
    | (_, _)                          -> Some(List.rev (iter(start, [])))
                
// P18 (**) Extract a slice from a list.
//Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of
//the original list (both limits included). Start counting the elements with 1.
//Example:?­ slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
//X = [c,d,e,f,g]

//P19 (**) Rotate a list N places to the left.
//Examples:
//?­ rotate([a,b,c,d,e,f,g,h],3,X).
//X = [d,e,f,g,h,a,b,c]
//?­ rotate([a,b,c,d,e,f,g,h],­2,X).
//X = [g,h,a,b,c,d,e,f]
//Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem P17



//P23 (**) Extract a given number of randomly selected elements from a list.
//The selected items shall be put into a result list.
//Example:
//?­ rnd_select([a,b,c,d,e,f,g,h],3,L).
//L = [e,d,a]
//Hint: Use the built­in random number generator random/2 and the result of problem P20.

//P24 (*) Lotto: Draw N different random numbers from the set 1..M.
//The selected numbers shall be put into a result list.
//Example:
//?­ rnd_select(6,49,L).
//L = [23,1,17,33,21,37]
//Hint: Combine the solutions of problems P22 and P23.

//P25 (*) Generate a random permutation of the elements of a list.Example:
//?­ rnd_permu([a,b,c,d,e,f],L).
//L = [b,a,d,c,e,f]
//Hint: Use the solution of problem P23.

// P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
// In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there
// are C(12,3) = 220 possibilities (C(N,K) denotes the well­known binomial coefficients). For pure
// mathematicians, this result may be great. But we want to really generate all the possibilities (via
// backtracking).
// Example:
// ?­ combination(3,[a,b,c,d,e,f],L).
// L = [a,b,c] ;
// L = [a,b,d] ;
// L = [a,b,e] ;
// ...

// P27 (**) Group the elements of a set into disjoint subsets.
// a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write
// a predicate that generates all the possibilities via backtracking.
// Example:
// ?­ group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
// G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
// ...
// b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will
// return a list of groups.
// Example:
// ?­ group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
// Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
// ...
// Note that we do not want permutations of the group members; i.e. [[aldo,beat],...] is the same solution as
// [[beat,aldo],...]. However, we make a difference between [[aldo,beat],[carla,david],...] and
// [[carla,david],[aldo,beat],...].
// You may find more about this combinatorial problem in a good book on discrete mathematics under the
// term "multinomial coefficients".

// P28 (**) Sorting a list of lists according to length of sublists
// a) We suppose that a list (InList) contains elements that are lists themselves. The objective is to sort theelements of InList according to their length. E.g. short lists first, longer lists later, or vice versa.
// Example:
// ?­ lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
// L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]
// b) Again, we suppose that a list (InList) contains elements that are lists themselves. But this time the
// objective is to sort the elements of InList according to their length frequency; i.e. in the default, where
// sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come
// later.
// Example:
//­ lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
// L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]
// Note that in the above example, the first two lists in the result L have length 4 and 1, both lengths appear
// just once. The third and forth list have length 3 which appears, there are two list of this length. And finally,
// the last three lists have length 2. This is the most frequent length.
