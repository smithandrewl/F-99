namespace F99

module BinaryTrees =
    type BinaryTree<'a> =
        | Node of value: 'a * left: BinaryTree<'a> * right: BinaryTree<'a>
        | Leaf of value: 'a

    (*
        P54 (*) Write a predicate istree which succeeds if and only if 
                its argument is a binary tree.  Not necessary since F# is strongly typed
    *)


            
