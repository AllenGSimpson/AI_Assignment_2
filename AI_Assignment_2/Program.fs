// Learn more about F# at http://fsharp.org

//System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ //here is how we set the current directory for F# interactive to where this file is (run without slashes ALT-ENTER)

open System
open Utility

[<EntryPoint>]
let main argv =
    let mazeDirectory= "../mazes"
    System.IO.Directory.EnumerateFiles(mazeDirectory, "*.txt")
    |> Seq.map readMaze







    0 // return an integer exit code
