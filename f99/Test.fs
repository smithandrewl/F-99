
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
        