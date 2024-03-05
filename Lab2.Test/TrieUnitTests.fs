module TrieUnitTests

open Xunit
open Trie

[<Fact>]
let ``Inserting into an empty trie should create a tree with a single key`` () =
    let insertedValue = 42
    let emptyTrie = Empty
    let trie = insert "abc" insertedValue emptyTrie

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
                | Node(value, _) -> Assert.Equal(insertedValue, unbox value.Value)
                | _ -> Assert.True(false, "Unexpected Node")
            | _ -> Assert.True(false, "Unexpected Node")
        | _ -> Assert.True(false, "Unexpected Node")
    | _ -> Assert.True(false, "Unexpected Node")

[<Fact>]
let ``Inserting into a non-empty trie should correctly add new keys`` () =
    let notEmptyTrie = insert "def" 33 Empty
    let trie = insert "abc" 42 notEmptyTrie

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
                | Node(value, _) -> Assert.Equal(42, unbox<int> value.Value)
                | _ -> Assert.True(false, "Unexpected Node")
            | _ -> Assert.True(false, "Unexpected Node")
        | _ -> Assert.True(false, "Unexpected Node")

        let child2 = Map.find 'd' children

        match child2 with
        | Node(_, childChildren) ->
            let grandChild = Map.find 'e' childChildren

            match grandChild with
            | Node(_, childChildChildren) ->
                let grandGrandChild = Map.find 'f' childChildChildren

                match grandGrandChild with
                | Node(value, _) -> Assert.Equal(33, unbox<int> value.Value)
                | _ -> Assert.True(false, "Unexpected Node")
            | _ -> Assert.True(false, "Unexpected Node")
        | _ -> Assert.True(false, "Unexpected Node")
    | _ -> Assert.True(false, "Unexpected Node")

[<Fact>]
let ``Inserting an existing key in a trie should replace its value`` () =
    let notEmptyTrie = insert "abc" 33 Empty
    let trie = insert "abc" 42 notEmptyTrie

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
                | Node(value, _) -> Assert.Equal(42, unbox<int> value.Value)
                | _ -> Assert.True(false, "Unexpected Node")
            | _ -> Assert.True(false, "Unexpected Node")
        | _ -> Assert.True(false, "Unexpected Node")
    | _ -> Assert.True(false, "Unexpected Node")

[<Fact>]
let ``Inserting multiple keys into a trie should create the correct structure`` () =
    let emptyTrie = Empty

    let trie =
        emptyTrie
        |> insert "A" 15
        |> insert "to" 7
        |> insert "ten" 12
        |> insert "tea" 3
        |> insert "ted" 4
        |> insert "i" 11
        |> insert "in" 5
        |> insert "inn" 9

    match trie with
    | Node(_, children) ->
        let childA = Map.find 'A' children

        match childA with
        | Node(value, _) -> Assert.Equal(15, unbox<int> value.Value)
        | _ -> Assert.True(false, "Unexpected Node")

        let childI = Map.find 'i' children

        match childI with
        | Node(value, _) -> Assert.Equal(11, unbox<int> value.Value)
        | _ -> Assert.True(false, "Unexpected Node")

        match childI with
        | Node(_, childIChildren) ->
            let childIN = Map.find 'n' childIChildren

            match childIN with
            | Node(value, _) -> Assert.Equal(5, unbox<int> value.Value)
            | _ -> Assert.True(false, "Unexpected Node")

            match childIN with
            | Node(_, childINChildren) ->
                let childINN = Map.find 'n' childINChildren

                match childINN with
                | Node(value, _) -> Assert.Equal(9, unbox<int> value.Value)
                | _ -> Assert.True(false, "Unexpected Node")
            | _ -> Assert.True(false, "Unexpected Node")
        | _ -> Assert.True(false, "Unexpected Node")

        let childT = Map.find 't' children

        match childT with
        | Node(_, childTChildren) ->
            let childTO = Map.find 'o' childTChildren

            match childTO with
            | Node(value, _) -> Assert.Equal(7, unbox<int> value.Value)
            | _ -> Assert.True(false, "Unexpected Node")

            let childTE = Map.find 'e' childTChildren

            match childTE with
            | Node(_, childINChildren) ->
                let childTEA = Map.find 'a' childINChildren

                match childTEA with
                | Node(value, _) -> Assert.Equal(3, unbox<int> value.Value)
                | _ -> Assert.True(false, "Unexpected Node")

                let childTEN = Map.find 'n' childINChildren

                match childTEN with
                | Node(value, _) -> Assert.Equal(12, unbox<int> value.Value)
                | _ -> Assert.True(false, "Unexpected Node")

                let childTED = Map.find 'd' childINChildren

                match childTED with
                | Node(value, _) -> Assert.Equal(4, unbox<int> value.Value)
                | _ -> Assert.True(false, "Unexpected Node")
            | _ -> Assert.True(false, "Unexpected Node")
        | _ -> Assert.True(false, "Unexpected Node")

    | _ -> Assert.True(false, "Unexpected Node")

[<Fact>]
let ``Finding a non-existent key in an empty trie should return None`` () =
    let findResult = Empty |> find "not-existed"
    Assert.Equal(None, findResult)

[<Fact>]
let ``Finding an existing key in a trie should return the correct value`` () =
    let insertedValue = 334
    let trie = Empty |> insert "existed" insertedValue
    let findResult = trie |> find "existed"
    Assert.Equal(insertedValue, unbox<int> findResult.Value)

[<Fact>]
let ``Removing a non-existent key from an empty trie should return empty trie`` () =
    let result = Empty |> remove "not-existed"
    Assert.Equal(result, Empty)

[<Fact>]
let ``Removing an existing key from a trie should result in the key not being found`` () =
    let key = "existed"
    let trie = Empty |> insert key 334 |> remove key
    let findRemovedKeyResult = trie |> find key
    Assert.Equal(findRemovedKeyResult, None)

[<Fact>]
let ``Filtering empty trie should return empty trie`` () =
    let filteredTrie = Empty |> filter (fun value -> (unbox<int> value) > 9)
    Assert.Equal(filteredTrie, Empty)

[<Fact>]
let ``Filtering trie should return trie with values satisfying the predicate`` () =
    let trie =
        Empty
        |> insert "apple" 1
        |> insert "banana" 2
        |> insert "cherry" 3
        |> insert "orange" 4

    let expected = Empty |> insert "banana" 2 |> insert "orange" 4

    let filteredTrie = trie |> filter (fun value -> unbox<int> value % 2 = 0)
    Assert.Equal(expected, filteredTrie)

[<Fact>]
let ``Mapping over an empty trie should return an empty trie`` () =
    let mappedTrie = map (fun x -> x * 2) Empty
    Assert.Equal(Empty, mappedTrie)

[<Fact>]
let ``Mapping over a non-empty trie should apply the function to each value`` () =
    let trie = Empty |> insert "a" 1 |> insert "b" 2 |> insert "c" 3

    let mappedTrie = trie |> map (fun x -> unbox<int> x * 2)

    let findResultA = mappedTrie |> find "a"
    let findResultB = mappedTrie |> find "b"
    let findResultC = mappedTrie |> find "c"

    Assert.Equal(findResultA, Some 2)
    Assert.Equal(findResultB, Some 4)
    Assert.Equal(findResultC, Some 6)

[<Fact>]
let ``Mapping over a trie with nested values should apply the function recursively`` () =
    let trie =
        Empty
        |> insert "a" 1
        |> insert "ab" 2
        |> insert "c" 3
        |> insert "cd" 4
        |> insert "cde" 5

    let mappedTrie = trie |> map (fun x -> unbox<int> x * 2)

    let findResultA = mappedTrie |> find "a"
    let findResultAB = mappedTrie |> find "ab"
    let findResultC = mappedTrie |> find "c"
    let findResultCD = mappedTrie |> find "cd"
    let findResultCDE = mappedTrie |> find "cde"

    Assert.Equal(findResultA, Some 2)
    Assert.Equal(findResultAB, Some 4)
    Assert.Equal(findResultC, Some 6)
    Assert.Equal(findResultCD, Some 8)
    Assert.Equal(findResultCDE, Some 10)

[<Fact>]
let ``Fold should return the initial state when applied to an empty Trie`` () =
    let state = 0
    let result = Empty |> fold (fun acc value -> box (unbox acc + unbox value)) state
    Assert.Equal(state, unbox result)

[<Fact>]
let ``Fold should accumulate values correctly in a Trie`` () =
    let state = 0
    let trie = Empty |> insert "a" 1 |> insert "b" 2 |> insert "c" 3
    let result = trie |> fold (fun acc value -> box (unbox acc + unbox value)) state
    Assert.Equal(6, unbox result)

[<Fact>]
let ``FoldRight should return the initial state when applied to an empty Trie`` () =
    let state = 0

    let result =
        Empty |> foldRight (fun value acc -> box (unbox acc + unbox value)) state

    Assert.Equal(state, unbox result)

[<Fact>]
let ``FoldRight should accumulate values correctly in a Trie`` () =
    let state = 0
    let trie = Empty |> insert "a" 1 |> insert "b" 2 |> insert "c" 3

    let result =
        trie |> foldRight (fun value acc -> box (unbox acc + unbox value)) state

    Assert.Equal(6, unbox result)

[<Fact>]
let ``Merge should combine two tries correctly`` () =
    let trie1 =
        Empty
        |> insert "a" 1
        |> insert "ab" 4
        |> insert "b" 2
        |> insert "c" 3
        |> insert "cd" 5

    let trie2 =
        Empty
        |> insert "a" 222
        |> insert "abc" 200
        |> insert "b" 334
        |> insert "ba" 3331
        |> insert "c" 0

    let expected =
        Empty
        |> insert "a" 1
        |> insert "ab" 4
        |> insert "abc" 200
        |> insert "b" 2
        |> insert "ba" 3331
        |> insert "c" 3
        |> insert "cd" 5

    let result = merge trie1 trie2

    Assert.Equal(expected, result)

[<Fact>]
let ``Merge should handle empty tries correctly`` () =
    let trie1 = Empty |> insert "abc" 1
    let trie2 = Empty

    let result = merge trie1 trie2

    Assert.Equal(trie1, result)

[<Fact>]
let ``Merge should handle two empty tries correctly`` () =
    let trie1 = Empty
    let trie2 = Empty

    let result = merge trie1 trie2

    Assert.Equal(Empty, result)

[<Fact>]
let ``MapToTrie should handle empty map`` () =
    let map = Map.empty
    let trie = mapToTrie map
    let expected = Empty
    Assert.Equal(expected, trie)

[<Fact>]
let ``MapToTrie should create trie with exact same values of map`` () =
    let map = Map.ofList [ ("key1", box 1); ("key2", 2); ("key3", 3) ]
    let trie = mapToTrie map
    let expected = Empty |> insert "key1" 1 |> insert "key2" 2 |> insert "key3" 3
    Assert.Equal(expected, trie)

[<Fact>]
let ``Empty trie is equal to empty trie`` () =
    let trie1 = Empty
    let trie2 = Empty
    equals trie1 trie2 |> Assert.True

[<Fact>]
let ``Tries with different keys are not equal`` () =
    let trie1 = Empty |> insert "some1" 32
    let trie2 = Empty |> insert "some2" 32
    equals trie1 trie2 |> Assert.False

[<Fact>]
let ``Tries with different values under same keys are not equal`` () =
    let trie1 = Empty |> insert "some1" 31
    let trie2 = Empty |> insert "some1" 32
    equals trie1 trie2 |> Assert.False

[<Fact>]
let ``Tries with same pairs are equal`` () =
    let trie1 = Empty |> insert "some1" 32 |> insert "some2" 33 |> insert "some3" 34
    let trie2 = Empty |> insert "some1" 32 |> insert "some2" 33 |> insert "some3" 34
    equals trie1 trie2 |> Assert.True

[<Fact>]
let ``Tries with same pairs but different lengths are not equal`` () =
    let trie1 = Empty |> insert "some1" 32 |> insert "some2" 33 |> insert "some3" 34
    let trie2 = Empty |> insert "some1" 32 |> insert "some2" 33
    equals trie1 trie2 |> Assert.False
