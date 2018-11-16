// Learn more about F# at http://fsharp.org

//System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ //here is how we set the current directory for F# interactive to where this file is (run without slashes ALT-ENTER)

open System
open Utility

[<EntryPoint>]
let main argv =
    let runMazeState =  //runMazeState will be...
        match argv.[0].ToLowerInvariant() with
        | "--simple"                            //|
        | "-s" -> Utility.runMazeStateSimple    //The Simple Maze   
        | "--forward"                           //or
        | "-f" -> Utility.runMazeStateForward   //The Forward Maze
        | unknown -> failwithf "I don't know what %s means" unknown //you gave it a different argument
    let mazeDirectory= if argv.Length >1 then argv.[1] else "../mazes"  //go fin the mazes
    printfn "\n================================================================================"    //print beutification lines
    System.IO.Directory.EnumerateFiles(mazeDirectory, "*.txt")
    |> Seq.map readMaze //get the maze
    |> Seq.map createMazeState  //create a mazeState
    |> Seq.map (search runMazeState)    //run the algorithim of your choice on every maze in ../mazes
    |> Seq.iter (function               //and look at each maze
        | None -> printfn "No valid mazes"  //if there was no solution say so, else..
        | Some (solution,count) -> printMazeState solution; printfn "================================================================================") //print beutification lines and the finished maze
    printfn ("All Mazes finished!")     //print the finish comment!


    0 // return an integer exit code
