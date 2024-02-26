module Tests

open Xunit
open Trie

[<Fact>]
let ``Adding a key to an empty trie``() =
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