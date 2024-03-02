module TrieUnitTests

open Xunit
open Trie

[<Fact>]
let ``Adding a key to an empty trie`` () =
    let emptyTrie = Empty
    let trie = add "abc" 42 emptyTrie

    match trie with
    | Node(_, children) ->
        let child = Map.find 'a' children

        match child with
        | Node(_, childChildren) ->
            let grandChild = Map.find 'b' childChildren

            match grandChild with
            | Node(_, childChildChildren) ->
                let grandGrandChild = Map.find 'c' childChildChildren

                match grandGrandChild with
                | Node(value, _) -> Assert.Equal(box 42, box value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)
    | _ -> Assert.True(false)

[<Fact>]
let ``Add key-value pair to an existing trie`` () =
    let NotEmptyTrie = add "def" 33 Empty
    let trie = add "abc" 42 NotEmptyTrie

    match trie with
    | Node(_, children) ->
        let child1 = Map.find 'a' children

        match child1 with
        | Node(_, childChildren) ->
            let grandChild = Map.find 'b' childChildren

            match grandChild with
            | Node(_, childChildChildren) ->
                let grandGrandChild = Map.find 'c' childChildChildren

                match grandGrandChild with
                | Node(value, _) -> Assert.Equal(box 42, box value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)

        let child2 = Map.find 'd' children

        match child2 with
        | Node(_, childChildren) ->
            let grandChild = Map.find 'e' childChildren

            match grandChild with
            | Node(_, childChildChildren) ->
                let grandGrandChild = Map.find 'f' childChildChildren

                match grandGrandChild with
                | Node(value, _) -> Assert.Equal(box 33, box value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)
    | _ -> Assert.True(false)


[<Fact>]
let ``Add key-value pair to an existing trie with repetitions`` () =
    let emptyTrie = Empty

    let trie =
        emptyTrie
        |> add "A" 15
        |> add "to" 7
        |> add "ten" 12
        |> add "tea" 3
        |> add "ted" 4
        |> add "i" 11
        |> add "in" 5
        |> add "inn" 9

    match trie with
    | Node(_, children) ->
        let childA = Map.find 'A' children

        match childA with
        | Node(value, _) -> Assert.Equal(box 15, value.Value)
        | _ -> Assert.True(false)
        
        let childI = Map.find 'i' children

        match childI with
        | Node(value, _) -> Assert.Equal(box 11, value.Value)
        | _ -> Assert.True(false)
        
        match childI with
        | Node(_, childIChildren) ->
            let childIN = Map.find 'n' childIChildren
            
            match childIN with
            | Node(value, _) -> Assert.Equal(box 5, value.Value)
            | _ -> Assert.True(false)
            
            match childIN with
            | Node(_, childINChildren) ->
                let childINN = Map.find 'n' childINChildren
                
                match childINN with
                | Node(value, _) -> Assert.Equal(box 9, value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)
        
        let childT = Map.find 't' children
        
        match childT with
        | Node(_, childTChildren) ->
            let childTO = Map.find 'o' childTChildren
            
            match childTO with
            | Node(value, _) -> Assert.Equal(box 7, value.Value)
            | _ -> Assert.True(false)
                        
            let childTE = Map.find 'e' childTChildren

            match childTE with
            | Node(_, childINChildren) ->
                let childTEA = Map.find 'a' childINChildren
                
                match childTEA with
                | Node(value, _) -> Assert.Equal(box 3, value.Value)
                | _ -> Assert.True(false)
                
                let childTEN = Map.find 'n' childINChildren
                
                match childTEN with
                | Node(value, _) -> Assert.Equal(box 12, value.Value)
                | _ -> Assert.True(false)
                
                let childTED = Map.find 'd' childINChildren
                
                match childTED with
                | Node(value, _) -> Assert.Equal(box 4, value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)
        
    | _ -> Assert.True(false)
