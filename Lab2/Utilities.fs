module Utilities


let listToString list =
    list |> List.map string |> String.concat ""
