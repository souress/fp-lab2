module Trie

open Utilities


type Trie<'T> =
    | Empty
    | Node of 'T option * Map<char, Trie<'T>>

let empty<'T> = Empty

let rec add (key: string) (value: 'T) (trie: Trie<'T>) =
    let keyList = Seq.toList key

    match keyList, trie with
    | [], Node(_, children) -> Node(Some value, children)
    | k :: ks, Empty ->

        let child =
            match ks.IsEmpty with
            | false -> add (listToString ks) value empty<'T>
            | true -> Node(Some value, Map.empty)

        let children = Map.add k child Map.empty
        Node(None, children)

    | k :: ks, Node(optValue, children) ->
        let child =
            match Map.tryFind k children with
            | Some childTrie -> childTrie
            | None -> empty<'T>

        let updatedChild =
            match ks.IsEmpty with
            | false -> add (listToString ks) value child
            | true -> Node(Some value, Map.empty)
            
        let updatedChildren = children |> Map.add k updatedChild
        Node(optValue, updatedChildren)
    | _, _ -> failwith "Invalid key or trie"
