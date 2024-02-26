module Tests

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
