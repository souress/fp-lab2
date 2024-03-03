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
            | false -> insert (listToString ks) value empty
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
            | false -> insert (listToString ks) value child
            | true -> Node(Some value, Map.empty)

        let updatedChildren = children |> Map.add k updatedChild
        Node(optValue, updatedChildren)
    | _, _ -> failwith "Invalid key or trie to insert"

let rec remove key trie =
    let keyList = Seq.toList key

    match keyList, trie with
    | [], Node(_, children) -> Node(None, children)
    | k :: ks, Node(optValue, children) ->
        match Map.tryFind k children with
        | Some childTrie ->
            let updatedChild = remove (listToString ks) childTrie

            let updatedChildren =
                match updatedChild with
                | Empty -> children |> Map.remove k
                | _ -> children |> Map.add k updatedChild

            Node(optValue, updatedChildren)
        | None -> Node(optValue, children)
    | _ :: _, Empty -> Empty
    | _, _ -> failwith "Invalid key or trie to remove"

let rec find key trie =
    let keyList = Seq.toList key

    match keyList, trie with
    | [], Node(optValue, _) -> optValue
    | k :: ks, Node(_, children) ->
        match Map.tryFind k children with
        | Some childTrie -> find (listToString ks) childTrie
        | None -> None
    | _, _ -> None

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
