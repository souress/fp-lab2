module TrieUnitTests

open Xunit
open Trie

[<Fact>]
let ``Вставка в пустой словарь`` () =
    let insertedValue = 42
    let emptyTrie = Empty
    let trie = add "abc" insertedValue emptyTrie

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
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)
    | _ -> Assert.True(false)

[<Fact>]
let ``Вставка в непустой словарь`` () =
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
                | Node(value, _) -> Assert.Equal(42, unbox<int> value.Value)
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
                | Node(value, _) -> Assert.Equal(33, unbox<int> value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)
    | _ -> Assert.True(false)

[<Fact>]
let ``Вставка по существующему ключу`` () =
    let NotEmptyTrie = add "abc" 33 Empty
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
                | Node(value, _) -> Assert.Equal(42, unbox<int> value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)
    | _ -> Assert.True(false)

[<Fact>]
let ``Вставка множества пар в словарь`` () =
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
        | Node(value, _) -> Assert.Equal(15, unbox<int> value.Value)
        | _ -> Assert.True(false)

        let childI = Map.find 'i' children

        match childI with
        | Node(value, _) -> Assert.Equal(11, unbox<int> value.Value)
        | _ -> Assert.True(false)

        match childI with
        | Node(_, childIChildren) ->
            let childIN = Map.find 'n' childIChildren

            match childIN with
            | Node(value, _) -> Assert.Equal(5, unbox<int> value.Value)
            | _ -> Assert.True(false)

            match childIN with
            | Node(_, childINChildren) ->
                let childINN = Map.find 'n' childINChildren

                match childINN with
                | Node(value, _) -> Assert.Equal(9, unbox<int> value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)

        let childT = Map.find 't' children

        match childT with
        | Node(_, childTChildren) ->
            let childTO = Map.find 'o' childTChildren

            match childTO with
            | Node(value, _) -> Assert.Equal(7, unbox<int> value.Value)
            | _ -> Assert.True(false)

            let childTE = Map.find 'e' childTChildren

            match childTE with
            | Node(_, childINChildren) ->
                let childTEA = Map.find 'a' childINChildren

                match childTEA with
                | Node(value, _) -> Assert.Equal(3, unbox<int> value.Value)
                | _ -> Assert.True(false)

                let childTEN = Map.find 'n' childINChildren

                match childTEN with
                | Node(value, _) -> Assert.Equal(12, unbox<int> value.Value)
                | _ -> Assert.True(false)

                let childTED = Map.find 'd' childINChildren

                match childTED with
                | Node(value, _) -> Assert.Equal(4, unbox<int> value.Value)
                | _ -> Assert.True(false)
            | _ -> Assert.True(false)
        | _ -> Assert.True(false)

    | _ -> Assert.True(false)

[<Fact>]
let ``Поиск значения по пустому словарю`` () =
    let findResult = Empty |> find "not-existed"
    Assert.Equal(None, findResult)

[<Fact>]
let ``Поиск существующего значения по словарю`` () =
    let insertedValue = 334
    let trie = Empty |> add "existed" insertedValue
    let findResult = trie |> find "existed"
    Assert.Equal(insertedValue, unbox<int> findResult.Value)

[<Fact>]
let ``Удаление значения по ключу из пустого словаря`` () =
    let emptyTrie = Empty
    let result = emptyTrie |> remove "not-existed"
    Assert.Equal(result, emptyTrie)

[<Fact>]
let ``Удаление значения по существующему ключу из словаря`` () =
    let key = "existed"
    let trie = Empty |> add key 334 |> remove key
    let findRemovedKeyResult = trie |> find key
    Assert.Equal(findRemovedKeyResult, None)
