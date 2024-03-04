module TriePropertyBasedTests

open FsCheck
open FsCheck.Xunit
open Trie

let maxValue = 100
let count = 10

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
