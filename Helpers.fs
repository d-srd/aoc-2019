module Helpers

let getInput day = 
    System.IO.File.ReadLines(sprintf "./Inputs/%i.txt" day)
    |> Seq.cast<string>