module Trie

open Utilities

type Trie<'T> =
    | Empty
    | Node of 'T option * Map<char, Trie<'T>>

let empty<'T> = Empty

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

let insertCharListKey (key: char list) value trie =
    insert (charListToString key) value trie

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

let removeCharListKey key trie = remove (charListToString key) trie

let rec find key trie =
    let keyList = Seq.toList key

    match keyList, trie with
    | [], Node(optValue, _) -> optValue
    | k :: ks, Node(_, children) ->
        match Map.tryFind k children with
        | Some childTrie -> find (charListToString ks) childTrie
        | None -> None
    | _, _ -> None

let findCharListKey (key: char list) trie = find (charListToString key) trie

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

let rec fold<'T, 'State> f state trie =
    match trie with
    | Empty -> state
    | Node(optValue, children) ->
        let newState =
            match optValue with
            | Some value -> f state value
            | None -> state

        Map.fold (fun acc _ -> fold f acc) newState children

let rec foldRight<'T, 'State> (f: obj -> obj -> obj) state trie =
    match trie with
    | Empty -> state
    | Node(optValue, children) ->
        let newState =
            match optValue with
            | Some value -> f state value
            | None -> state

        Map.foldBack (fun _ s state -> foldRight f state s) children newState

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

let mapToTrie map =
    (Empty, map) ||> Map.fold (fun acc k v -> insert k v acc)

let rec equals (trie1: Trie<'T>) (trie2: Trie<'T>) =
    match trie1, trie2 with
    | Empty, Empty -> true
    | Node(value1, children1), Node(value2, children2) ->
        let childrenEqual =
            let childKeys1 = Map.keys children1 |> Seq.toList
            let childKeys2 = Map.keys children2 |> Seq.toList

            let rec compareChildren keys1 keys2 =
                match keys1, keys2 with
                | [], [] -> true
                | k1 :: ks1, k2 :: ks2 ->
                    match equalValues k1 k2 with
                    | true ->
                        let child1 = Map.find k1 children1
                        let child2 = Map.find k2 children2

                        if equals child1 child2 then
                            compareChildren ks1 ks2
                        else
                            false
                    | _ -> false
                | _, _ -> false

            compareChildren childKeys1 childKeys2

        (equalValues value1 value2) && childrenEqual
    | _, _ -> false
