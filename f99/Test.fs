namespace f99tests
open System
open NUnit.Framework
open f99

[<TestFixture>]
type Test() = class
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
    member self.F01Last () =
        Assert.AreEqual(Some('d'), my_last ABCD)

    [<Test>]
    member self.F01One () =
        Assert.AreEqual(Some('a'), my_last ['a'])
        
    [<Test>]
    member self.F01Empty () =
        Assert.AreEqual(None, my_last [])

    [<Test>]
    member self.F02Empty () =
        Assert.AreEqual(None, next_to_last [])

    [<Test>]
    member self.F02OneElement () =
        Assert.AreEqual(None, next_to_last ['a'])

    [<Test>]
    member self.F02TwoElements () =
        Assert.AreEqual(Some(1), next_to_last OneToTwo)

    [<Test>]
    member self.F02MultipleElements () =
        Assert.AreEqual(Some(4), next_to_last OneToFive)

    [<Test>]
    member self.F03Empty () =
        Assert.AreEqual(None, element_at [] 0)

    [<Test>]
    member self.F03OneElement () =
        Assert.AreEqual(Some(1), element_at [1] 0)
        
    [<Test>]
    member self.F03FiveElements () =
        Assert.AreEqual(Some(4), element_at OneToFive 3)

    [<Test>]
    member self.F03OutOfBounds () =
        Assert.AreEqual(None, element_at OneToFour 5)

    [<Test>]
    member self.F04Empty () =
        Assert.AreEqual(0, num_of_elements [])

    [<Test>]
    member self.F04One () =
        Assert.AreEqual(1, num_of_elements [1])

    [<Test>]
    member self.F04Five () =
        Assert.AreEqual(5, num_of_elements OneToFive)

    [<Test>]
    member self.F05Empty () =
        Assert.AreEqual([], rev [])

    [<Test>]
    member self.F05One () =
        Assert.AreEqual([2], rev [2])
        
    [<Test>]
    member self.F05Three () =
        Assert.AreEqual([3; 2; 1], rev OneToThree)

    [<Test>]
    member self.F06Empty () =
        Assert.AreEqual(true, is_pal [])

    [<Test>]
    member self.F06OneItem () =
        Assert.AreEqual(true, is_pal ['a'])

    [<Test>]
    member self.F06Pal () =
        Assert.AreEqual(true, is_pal XAMAX)

    [<Test>]
    member self.F06NonPal () =
        Assert.AreEqual(false, is_pal ['a' .. 'c'])

        
    [<Test>]
    member self.F08Empty () =
        Assert.AreEqual([], remove_duplicates [])


    [<Test>]
    member self.F08One () = 
        Assert.AreEqual([1], remove_duplicates [1])
        
    [<Test>]
    member self.F08TwoDup () =
        Assert.AreEqual([1], remove_duplicates [1; 1])

    [<Test>]
    member self.F08ManyDup () =
        Assert.AreEqual(['a'; 'b'; 'c'; 'a'; 'd'; 'e'], remove_duplicates Dups)

    [<Test>]
    member self.F09Pack () =
        Assert.AreEqual(DupsPacked, pack Dups)

    [<Test>]
    member self.F10Encode () =
        Assert.AreEqual(DupsEncoded, encode Dups)

    [<Test>]
    member self.F10EncodeOneGroup () =
        Assert.AreEqual([(5, 'a')], encode ['a'; 'a'; 'a'; 'a'; 'a'])
        
    [<Test>]
    member self.F10EncodeEmpty () =
        Assert.AreEqual(true, List.isEmpty (encode []))

    [<Test>]
    member self.F12DecodeEmpty () = 
        Assert.AreEqual(true, List.isEmpty(decode []))

    [<Test>]
    member self.F12DecodeDups () =
        Assert.AreEqual(DupsPacked, decode DupsEncoded)
        

    [<Test>]
    member self.F13EncodeDirect () =
        Assert.AreEqual(DupsEncoded, encodeDirect Dups)

    [<Test>]
    member self.F13EncodeDirectOneGroup () =
        Assert.AreEqual([(5, 'a')], encodeDirect ['a'; 'a'; 'a'; 'a'; 'a'])
        
    [<Test>]
    member self.F13EncodeDirectEmpty () =
        Assert.AreEqual(true, List.isEmpty (encodeDirect []))
        
    [<Test>]
    member self.F14DuplicateEmpty () =
        Assert.AreEqual([], duplic [])

    [<Test>]
    member self.F14DuplicateSingle () =
        Assert.AreEqual(['a'; 'a'], duplic ['a'])            

    [<Test>]
    member self.F14DuplicateMany () =
        let input = ['a'; 'a'; 'b'; 'b'; 
                     'c'; 'c'; 'd'; 'd']
        
        let expected = ['a'; 'a'; 'a'; 'a';
                        'b'; 'b'; 'b'; 'b';
                        'c'; 'c'; 'c'; 'c'; 
                        'd'; 'd'; 'd'; 'd' ]
        
        Assert.AreEqual(expected, duplic input)

    [<Test>]
    member self.F15DuplicateEmpty () =
        Assert.AreEqual(true, List.isEmpty (dupli([], 10)))

    [<Test>]
    member self.F15DuplicateTwo () =
        let expected = ['a'; 'a'; 'b'; 'b']
        
        Assert.AreEqual(expected, dupli (['a'; 'b'], 2))

    [<Test>]
    member self.F15DuplicateMany () =

        let expected = ['a'; 'a'; 'a'; 
                        'b'; 'b'; 'b'; 
                        'c'; 'c'; 'c']
        
        Assert.AreEqual(expected, dupli(['a'; 'b'; 'c'], 3))

    [<Test>]
    member self.F15DuplicateSeveralZeroTimes () =
        Assert.AreEqual(true, List.isEmpty(dupli(['a'; 'b'; 'c';], 0)))

    [<Test>]
    member self.F16Drop () =
        Assert.AreEqual(['a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'j'], drop(['a' .. 'j'],3))

    [<Test>]
    member self.F16DropEmpty () =
        Assert.AreEqual([], drop([], 3))

    [<Test>]
    member self.F16DropEvery () =
        Assert.AreEqual(true, drop(['a' .. 'z'], 1).IsEmpty)

    [<Test>]
    member self.F17SplitEmpty () = 
        let actual = split([], 0).Value
        
        Assert.AreEqual(true, (fst actual).IsEmpty)
        Assert.AreEqual(true, (snd actual).IsEmpty)

    [<Test>]
    member self.F17SplitOneStart () =
        let expected = ([], ['a'])
        let actual   = split(['a'],0).Value
        
        Assert.AreEqual(true, (fst actual).IsEmpty)
        Assert.AreEqual(snd expected, snd actual) 

    [<Test>]
    member self.F17SplitOneEnd () =
        let expected = (['a'], [])
        let actual   = split(['a'],1).Value
        
        Assert.AreEqual(fst   expected, fst actual)
        Assert.AreEqual(true, (snd actual).IsEmpty)
        
    [<Test>]
    member self.F17SplitTwoMiddle () =
        let expected = (['a'], ['b'])
        let actual   = split(['a'; 'b'], 1).Value
        
        Assert.AreEqual(fst expected, fst actual)
        Assert.AreEqual(snd expected, snd actual)

    [<Test>]
    member self.F17SplitFiveAtThree() =
        let expected = (['a' .. 'c'], ['d'; 'e'])
        let actual   = split(['a' .. 'e'], 3).Value
        
        Assert.AreEqual(fst expected, fst actual)
        Assert.AreEqual(snd expected, snd expected)
    
    [<Test>]
    member self.F20RemoveAtEmptyList() =
        Assert.AreEqual([], remove_at([], 0))
        Assert.AreEqual([], remove_at([], 10))
    
    [<Test>]
    member self.F20RemoveAtListOfOne() =
        Assert.AreEqual(true,  remove_at(['a'], 0).IsEmpty)
        Assert.AreEqual(['a'], remove_at(['a'], 1))
        Assert.AreEqual(['a'], remove_at(['a'], 10))
    
    [<Test>]
    member self.F20RemoveAtListOfTwo() =
        Assert.AreEqual(['b'],           remove_at(['a'; 'b'], 0))
        Assert.AreEqual(['a'],           remove_at(['a'; 'b'], 1))
        Assert.AreEqual(['a'; 'b'],      remove_at(['a'; 'b'], 2))
        Assert.AreEqual(['a'; 'b'],      remove_at(['a'; 'b'], 10))
        Assert.AreEqual(['a'; 'c'; 'd'], remove_at(['a'; 'b'; 'c'; 'd'], 1))
end
       