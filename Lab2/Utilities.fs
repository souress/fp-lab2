module Utilities


let charListToString (list: char list) =
    list |> List.map string |> String.concat ""

let createMap (keys: 'K list) (values: 'V list) : Map<'K, 'V> =
    List.zip keys values |> List.fold (fun acc (k, v) -> Map.add k v acc) Map.empty
