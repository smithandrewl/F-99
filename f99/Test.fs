
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
