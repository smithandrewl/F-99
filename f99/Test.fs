namespace f99tests
open System
open NUnit.Framework
open f99

[<TestFixture>]
type Test() = 
        let OneToFive   = [1 .. 5]
        let OneToFour   = [1 .. 4]
        let OneToThree  = [1 .. 3]
        let OneToTwo    = [1 .. 2]
                
        let ABCD   = ['a' .. 'd']
        let XAMAX  = ['x'; 'a'; 'm'; 'a'; 'x']
     
        let Dups = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 
                    'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']
        
        let DupsPacked  = [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; 
                           ['a'; 'a']; ['d']; ['e'; 'e'; 'e'; 'e']]
                            
        let DupsEncoded = [(4, 'a'); (1, 'b'); (2, 'c');
                           (2, 'a'); (1, 'd'); (4, 'e')]
        
        [<Test>]
        abstract F01Last : unit -> unit
        [<Test>]
        default this.F01Last () =
            Assert.AreEqual(Some('d'), my_last ABCD)
        
        [<Test>]
        abstract F01One : unit -> unit
        [<Test>]
        default this.F01One () =
            Assert.AreEqual(Some('a'), my_last ['a'])
            
        [<Test>]
        abstract F01Empty : unit -> unit
        [<Test>]
        default this.F01Empty () =
            Assert.AreEqual(None, my_last [])
        
        [<Test>]
        abstract F02Empty : unit -> unit        
        [<Test>]
        default this.F02Empty () =
            Assert.AreEqual(None, next_to_last [])
        
        [<Test>]
        abstract F02OneElement : unit -> unit
        [<Test>]
        default this.F02OneElement () =
            Assert.AreEqual(None, next_to_last ['a'])
        
        [<Test>]
        abstract F02TwoElements : unit -> unit
        [<Test>]
        default this.F02TwoElements () =
            Assert.AreEqual(Some(1), next_to_last OneToTwo)
        
        [<Test>]
        abstract F02MultipleElements : unit -> unit
        [<Test>]
        default this.F02MultipleElements () =
            Assert.AreEqual(Some(4), next_to_last OneToFive)
        
        [<Test>]
        abstract F03Empty : unit -> unit
        [<Test>]
        default this.F03Empty () =
            Assert.AreEqual(None, element_at [] 0)
        
        [<Test>]
        abstract F03OneElement : unit -> unit
        [<Test>]
        default this.F03OneElement () =
            Assert.AreEqual(Some(1), element_at [1] 0)
            
        [<Test>]
        abstract F03FiveElements : unit -> unit
        [<Test>]
        default this.F03FiveElements () =
            Assert.AreEqual(Some(4), element_at OneToFive 3)
        
        [<Test>]
        abstract F03OutOfBounds : unit -> unit
        [<Test>]
        default this.F03OutOfBounds () =
            Assert.AreEqual(None, element_at OneToFour 5)
        
        [<Test>]
        abstract F04Empty : unit -> unit
        [<Test>]
        default this.F04Empty () =
            Assert.AreEqual(0, num_of_elements [])

        [<Test>]
        abstract F04One : unit -> unit
        [<Test>]
        default this.F04One () =
            Assert.AreEqual(1, num_of_elements [1])
        
        [<Test>]
        abstract F04Five: unit -> unit
        [<Test>]
        default this.F04Five () =
            Assert.AreEqual(5, num_of_elements OneToFive)
        
        [<Test>]
        abstract F05Empty: unit -> unit
        [<Test>]
        default this.F05Empty () =
            Assert.AreEqual([], rev [])
        
        [<Test>]
        abstract F05One: unit -> unit
        [<Test>]
        default this.F05One () =
            Assert.AreEqual([2], rev [2])
            
        [<Test>]
        abstract F05Three: unit -> unit
        [<Test>]
        default this.F05Three () =
            Assert.AreEqual([3; 2; 1], rev OneToThree)
        
        [<Test>]
        abstract F06Empty : unit -> unit
        [<Test>]
        default this.F06Empty () =
            Assert.AreEqual(true, is_pal [])
        
        [<Test>]
        abstract F06OneItem : unit -> unit
        [<Test>]
        default this.F06OneItem () =
            Assert.AreEqual(true, is_pal ['a'])
        
        [<Test>]
        abstract F06Pal : unit -> unit
        [<Test>]
        default this.F06Pal () =
            Assert.AreEqual(true, is_pal XAMAX)
        
        [<Test>]
        abstract F06NonPal: unit -> unit
        [<Test>]
        default this.F06NonPal () =
            Assert.AreEqual(false, is_pal ['a' .. 'c'])
        
            
        [<Test>]
        abstract F08Empty: unit -> unit
        [<Test>]
        default this.F08Empty () =
            Assert.AreEqual([], remove_duplicates [])
        
        
        [<Test>]
        abstract F08One: unit -> unit
        [<Test>]
        default this.F08One () = 
            Assert.AreEqual([1], remove_duplicates [1])
            
        [<Test>]
        abstract F08TwoDup : unit -> unit
        [<Test>]
        default this.F08TwoDup () =
            Assert.AreEqual([1], remove_duplicates [1; 1])
        
        [<Test>]
        abstract F08ManyDup : unit -> unit
        [<Test>]
        default this.F08ManyDup () =
            Assert.AreEqual(['a'; 'b'; 'c'; 'a'; 'd'; 'e'], remove_duplicates Dups)
        
        [<Test>]
        abstract F09Pack : unit -> unit
        [<Test>]
        default this.F09Pack () =
            Assert.AreEqual(DupsPacked, pack Dups)
        
        [<Test>]
        abstract F10Encode: unit -> unit
        [<Test>]
        default this.F10Encode () =
            Assert.AreEqual(DupsEncoded, encode Dups)
        
        [<Test>]
        abstract F10EncodeOneGroup: unit -> unit
        [<Test>]
        default this.F10EncodeOneGroup () =
            Assert.AreEqual([(5, 'a')], encode ['a'; 'a'; 'a'; 'a'; 'a'])
            
        [<Test>]
        abstract F10EncodeEmpty: unit -> unit
        [<Test>]
        default this.F10EncodeEmpty () =
            Assert.AreEqual(true, List.isEmpty (encode []))
        
        [<Test>]
        abstract F12DecodeEmpty: unit -> unit
        [<Test>]
        default this.F12DecodeEmpty () = 
            Assert.AreEqual(true, List.isEmpty(decode []))
        
        [<Test>]
        abstract F12DecodeDups: unit -> unit
        [<Test>]
        default this.F12DecodeDups () =
            Assert.AreEqual(DupsPacked, decode DupsEncoded)
            
        
        [<Test>]
        abstract F13EncodeDirect: unit -> unit
        [<Test>]
        default this.F13EncodeDirect () =
            Assert.AreEqual(DupsEncoded, encodeDirect Dups)
        
        [<Test>]
        abstract F13EncodeDirectOneGroup: unit -> unit
        [<Test>]
        default this.F13EncodeDirectOneGroup () =
            Assert.AreEqual([(5, 'a')], encodeDirect ['a'; 'a'; 'a'; 'a'; 'a'])
            
        [<Test>]
        abstract F13EncodeDirectEmpty: unit -> unit
        [<Test>]
        default this.F13EncodeDirectEmpty () =
            Assert.AreEqual(true, List.isEmpty (encodeDirect []))
            
        [<Test>]
        abstract F14DuplicateEmpty: unit -> unit
        [<Test>]
        default this.F14DuplicateEmpty () =
            Assert.AreEqual([], duplic [])
        
        [<Test>]
        abstract F14DuplicateSingle: unit -> unit
        [<Test>]
        default this.F14DuplicateSingle () =
            Assert.AreEqual(['a'; 'a'], duplic ['a'])            
        
        [<Test>]
        abstract F14DuplicateMany: unit -> unit
        [<Test>]
        default this.F14DuplicateMany () =
            let input = ['a'; 'a'; 'b'; 'b'; 
                         'c'; 'c'; 'd'; 'd']
            
            let expected = ['a'; 'a'; 'a'; 'a';
                            'b'; 'b'; 'b'; 'b';
                            'c'; 'c'; 'c'; 'c'; 
                            'd'; 'd'; 'd'; 'd' ]
            
            Assert.AreEqual(expected, duplic input)
        
        [<Test>]
        abstract F15DuplicateEmpty: unit -> unit
        [<Test>]
        default this.F15DuplicateEmpty () =
            Assert.AreEqual(true, List.isEmpty (dupli([], 10)))
        
        [<Test>]
        abstract F15DuplicateTwo: unit -> unit
        [<Test>]
        default this.F15DuplicateTwo () =
            let expected = ['a'; 'a'; 'b'; 'b']
            
            Assert.AreEqual(expected, dupli (['a'; 'b'], 2))
        
        [<Test>]
        abstract F15DuplicateMany: unit -> unit
        [<Test>]
        default this.F15DuplicateMany () =
        
            let expected = ['a'; 'a'; 'a'; 
                            'b'; 'b'; 'b'; 
                            'c'; 'c'; 'c']
            
            Assert.AreEqual(expected, dupli(['a'; 'b'; 'c'], 3))
        
        [<Test>]
        abstract F15DuplicateSeveralZeroTimes : unit -> unit
        [<Test>]
        default this.F15DuplicateSeveralZeroTimes () =
            Assert.AreEqual(true, List.isEmpty(dupli(['a'; 'b'; 'c';], 0)))
        