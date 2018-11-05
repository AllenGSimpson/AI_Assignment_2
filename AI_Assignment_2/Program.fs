// Learn more about F# at http://fsharp.org

//System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ //here is how we set the current directory for F# interactive to where this file is (run without slashes ALT-ENTER)

open System
open Utility
open System

[<EntryPoint>]
let main argv =
    let mazeDirectory= "../mazes"
    System.IO.Directory.EnumerateFiles(mazeDirectory, "*.txt")
    |> Seq.map readMaze
    |> Seq.map createMazeState
    |> Seq.map (search runMazeStateForward)
    |> Seq.map (function
        | None -> printf "No valid mazes"
        | _ -> printf "Solution found!")
    |> ignore

    









    0 // return an integer exit code
