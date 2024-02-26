open Utilities

module Trie =
    type Trie<'a> =
        | Empty
        | Node of 'a option * Map<char, Trie<'a>>

    let empty<'a> = Empty

    let rec add (key: string) (value: 'a) (trie: Trie<'a>) =
        let keyList = Seq.toList key

        match keyList, trie with
        | [], Node(_, children) -> Node(Some value, children)
        | k :: ks, Node(optValue, children) ->
            let child =
                match Map.tryFind k children with
                | Some childTrie -> childTrie
                | None -> empty<'a>

            let updatedChild = add (listToString ks) value child
            let updatedChildren = children |> Map.add k updatedChild
            Node(optValue, updatedChildren)
        | _, _ -> failwith "Invalid key or trie"
