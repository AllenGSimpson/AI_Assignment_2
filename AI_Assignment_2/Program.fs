// Learn more about F# at http://fsharp.org

//System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ //here is how we set the current directory for F# interactive to where this file is (run without slashes ALT-ENTER)

open System
open Utility

[<EntryPoint>]
let main argv =
    let runMazeState =
        match argv.[0].ToLowerInvariant() with
        | "--simple"
        | "-s" -> Utility.runMazeStateSimple
        | "--forward"
        | "-f" -> Utility.runMazeStateForward
        | unknown -> failwithf "I don't know what %s means" unknown
    let mazeDirectory= if argv.Length >1 then argv.[1] else "../mazes"
    System.IO.Directory.EnumerateFiles(mazeDirectory, "*.txt")
    |> Seq.map readMaze
    |> Seq.map createMazeState
    |> Seq.map (search runMazeState)
    |> Seq.iter (function
        | None -> printfn "No valid mazes"
        | Some (solution,count) -> printfn "Solution found in (%d) steps" count; printMazeState solution)
    printfn ("All Mazes finished!")

    

(*
    //"../mazes/4x4maze.txt"
    
    argv.[1]
    |> readMaze
    |> createMazeState
    |> search runMazeState
    |> function
        | None -> printfn("No solution")
        | Some (solution,count) -> printfn "Solution found in (%d) steps" count; printMazeState solution
                


                *)






    0 // return an integer exit code
