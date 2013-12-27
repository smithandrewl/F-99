
namespace f99tests
open System
open NUnit.Framework
open f99

[<TestFixture>]
type Test() = 
        [<Test>]
        abstract F01Last : unit -> unit
        [<Test>]
        default this.F01Last () =
            let input = ['a'; 'b'; 'c'; 'd']
            let expected = Some('d')
            Assert.AreEqual(expected, my_last input)
        
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
            Assert.AreEqual(Some(1), next_to_last [1; 2])
        
        [<Test>]
        abstract F02MultipleElements : unit -> unit
        [<Test>]
        default this.F02MultipleElements () =
            Assert.AreEqual(Some(4), next_to_last [1; 2; 3; 4; 5])
        
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
            Assert.AreEqual(Some(4), element_at [1; 2; 3; 4; 5] 3)
        
        [<Test>]
        abstract F03OutOfBounds : unit -> unit
        [<Test>]
        default this.F03OutOfBounds () =
            Assert.AreEqual(None, element_at [1; 2; 3; 4] 5)
        
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
            Assert.AreEqual(5, num_of_elements [1; 2; 3; 4; 5])
        
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
            Assert.AreEqual([3; 2; 1], rev [1; 2; 3])
        
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
            Assert.AreEqual(true, is_pal ['x'; 'a'; 'm'; 'a'; 'x'])
        
        [<Test>]
        abstract F06NonPal: unit -> unit
        [<Test>]
        default this.F06NonPal () =
            Assert.AreEqual(false, is_pal ['a'; 'b'; 'c'])
        
            
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
            Assert.AreEqual(['a'; 'b'; 'c'; 'a'; 'd'; 'e'], remove_duplicates ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e'])
        
        [<Test>]
        abstract F09Pack : unit -> unit
        [<Test>]
        default this.F09Pack () =
            let expected = [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; ['a'; 'a']; ['d']; ['e'; 'e'; 'e'; 'e']]
            let input    = ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']
            
            Assert.AreEqual(expected, pack input)