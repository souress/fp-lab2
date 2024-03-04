# Функциональное программирование. Лабораторная работа №2.

## Вариант
Prefix tree dict.


## Цель
Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

## Требования
Требования:

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая); 
    - структура должна быть моноидом.

2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

## Реализация
- Словарь
```fsharp
type Trie<'T> =
    | Empty
    | Node of 'T option * Map<char, Trie<'T>>

let empty<'T> = Empty
```
- Добавление элемента
```fsharp
let rec insert key value trie =
    let keyList = Seq.toList key

    match keyList, trie with
    | [], Node(_, children) -> Node(Some value, children)
    | k :: ks, Empty ->
        let child =
            match ks.IsEmpty with
            | false -> insert (charListToString ks) value empty
            | true -> Node(Some value, Map.empty)

        let children = Map.add k child Map.empty
        Node(None, children)

    | k :: ks, Node(optValue, children) ->
        let child =
            match Map.tryFind k children with
            | Some childTrie -> childTrie
            | None -> empty

        let updatedChild =
            match ks.IsEmpty with
            | false -> insert (charListToString ks) value child
            | true -> Node(Some value, Map.empty)

        let updatedChildren = children |> Map.add k updatedChild
        Node(optValue, updatedChildren)
    | _, _ -> failwith "Invalid key or trie to insert"
```
- Удаление элемента
```fsharp
let rec remove key trie =
    let keyList = Seq.toList key

    match keyList, trie with
    | [], Node(_, children) -> Node(None, children)
    | k :: ks, Node(optValue, children) ->
        match Map.tryFind k children with
        | Some childTrie ->
            let updatedChild = remove (charListToString ks) childTrie

            let updatedChildren =
                match updatedChild with
                | Empty -> children |> Map.remove k
                | _ -> children |> Map.add k updatedChild

            Node(optValue, updatedChildren)
        | None -> Node(optValue, children)
    | _ :: _, Empty -> Empty
    | _, _ -> failwith "Invalid key or trie to remove"
```
- Поиск элемента
```fsharp
let rec find key trie =
    let keyList = Seq.toList key

    match keyList, trie with
    | [], Node(optValue, _) -> optValue
    | k :: ks, Node(_, children) ->
        match Map.tryFind k children with
        | Some childTrie -> find (charListToString ks) childTrie
        | None -> None
    | _, _ -> None
```
- Фильтрация
```fsharp
let rec filter predicate trie =
    match trie with
    | Empty -> Empty
    | Node(optValue, children) ->
        let filteredOptValue =
            match optValue with
            | Some value when predicate value -> Some value
            | _ -> None

        let filteredChildren =
            children
            |> Map.map (fun _ -> filter predicate)
            |> Map.filter (fun _ childTrie -> childTrie <> Empty)

        match filteredOptValue, filteredChildren.IsEmpty with
        | None, true -> Empty
        | _ -> Node(filteredOptValue, filteredChildren)
```
- Отображение
```fsharp
let rec map f trie =
    match trie with
    | Empty -> Empty
    | Node(optValue, children) ->
        let mappedOptValue =
            match optValue with
            | Some value -> Some(f value)
            | None -> None

        let mappedChildren =
            children
            |> Map.map (fun _ -> map f)
            |> Map.filter (fun _ childTrie -> childTrie <> Empty)

        Node(mappedOptValue, mappedChildren)
```
- Левая свертка
```fsharp
let rec fold<'T, 'State> f state trie =
    match trie with
    | Empty -> state
    | Node(optValue, children) ->
        let newState =
            match optValue with
            | Some value -> f state value
            | None -> state

        Map.fold (fun acc _ -> fold f acc) newState children
```
- Правая свертка
```fsharp
let rec foldRight<'T, 'State> (f: obj -> obj -> obj) state trie =
    match trie with
    | Empty -> state
    | Node(optValue, children) ->
        let newState =
            match optValue with
            | Some value -> f state value
            | None -> state

        Map.foldBack (fun _ s state -> foldRight f state s) children newState
```
- Слияние двух словарей
(если ключи совпадают берется значение из левого словаря)
```fsharp
let rec merge (trie1: Trie<'T>) (trie2: Trie<'T>) : Trie<'T> =
    match trie1, trie2 with
    | Empty, _ -> trie2
    | _, Empty -> trie1
    | Node(value1, children1), Node(value2, children2) ->
        let mergedValue =
            match value1, value2 with
            | Some v1, None -> Some v1
            | _, Some v2 -> Some v2
            | None, None -> None

        let mergedChildren =
            Map.fold
                (fun acc k v ->
                    match Map.tryFind k acc with
                    | Some childTrie -> Map.add k (merge v childTrie) acc
                    | None -> Map.add k v acc)
                children1
                children2

        Node(mergedValue, mergedChildren)
```

## Тестирование
### Unit тесты
```fsharp
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
```

### Property-based тесты
Для написания использовалась библиотека FsCheck.

Генераторы:
```fsharp
let generateKeyValuePair: Gen<char list * int> =
    Gen.zip Arb.generate<char list> Arb.generate<int>

let PairsGenerator: Gen<(char list * int) list> =
    gen {
        let pairs =
            generateKeyValuePair
            |> Gen.sample maxValue count
            |> Seq.toList
            |> List.filter (fst >> List.isEmpty >> not)

        return pairs
    }

let TrieGenerator: Gen<Trie<obj>> =
    gen {
        let pairs =
            PairsGenerator |> Gen.eval 10 (Random.newSeed () |> Random.stdSplit |> snd)

        let keys = pairs |> List.map (fst >> Utilities.charListToString)
        let values = pairs |> List.map (snd >> box<int>)

        let trie = mapToTrie (Utilities.createMap keys values)

        return trie
    }

type TrieArbitrary =
    static member Arbitrary: Arbitrary<Trie<obj>> = Arb.fromGen TrieGenerator

type PairsArbitrary =
    static member Arbitrary: Arbitrary<(char list * int) list> = Arb.fromGen PairsGenerator
```

```fsharp
[<Properties(Arbitrary = [| typeof<TrieArbitrary> |])>]
type TrieProperties() =
    [<Property>]
    let ``Monoid: associativity`` (trie1: Trie<obj>, trie2: Trie<obj>, trie3: Trie<obj>) =
        let mergedTrie1 = merge (merge trie1 trie2) trie3
        let mergedTrie2 = merge trie1 (merge trie2 trie3)

        mergedTrie1 = mergedTrie2

    [<Property>]
    let ``Monoid: identity`` (trie: Trie<obj>) =
        let trieAfterInsertEmpty = merge trie Empty
        let emptyAfterInsertTrie = merge Empty trie

        trie = trieAfterInsertEmpty && trie = emptyAfterInsertTrie

    [<Property(Arbitrary = [| typeof<PairsArbitrary> |])>]
    let ``Inserted values should be found`` (pairs: (char list * int) list) (trie: Trie<obj>) =
        let insertAllPairs (pairs: (char list * int) list) (trie: Trie<obj>) =
            List.fold (fun acc (key, value) -> insertCharListKey key value acc) trie pairs

        let insertedTrie = insertAllPairs pairs trie

        let results =
            pairs
            |> List.map (fun pair ->
                let foundValue = findCharListKey (fst pair) insertedTrie |> Option.get |> unbox<int>
                snd pair = foundValue)

        List.forall id results
```

### Отчет инструмента тестирования


## Вывод

В результате выполнения данной лабораторной работы я узнал о property-based тестировании, научился писать генераторы и пользовательские структуры.