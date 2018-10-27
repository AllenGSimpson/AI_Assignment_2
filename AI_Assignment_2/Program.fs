// Learn more about F# at http://fsharp.org

//System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ //here is how we set the current directory for F# interactive to where this file is (run without slashes ALT-ENTER)

open System
open Utility
open System

[<EntryPoint>]
let main argv =
    let mazeDirectory= "../mazes"
    System.IO.Directory.EnumerateFiles(mazeDirectory, "*.txt")
    |> Seq.map readMazeAsArray

    //Function to check if potential position is valid in maze array
    let validArrayPosition x y =
        if x < 0 || x >= m2.Length || y < 0 || y >= m2.[0].Length then false //Assuming m2 is what we save our maze as. False if out-of-bounds
        else true

    //Function to check if the position hasn't already been filled
    let notFilledPosition x y =
        if not (m2.[x,y].Equals('_')) then false
        else true

    //Checks the 4 adjacent squares for 'colors' and counts how many share with potential move square
    let adjSpaceColorCheck x y = 
        let m' : char[,] = m2.clone() //Is this necessary??
        let m0 = m'[ x , y ] //head
        let m1 = m'[ x , y-1 ] //up
        let m2 = m'[ x , y+1 ] //down
        let m3 = m'[ x+1 , y ] //right
        let m4 = m'[ x+1 , y ] //left
        let list1 = [m1;m2;m3;m4] //Save results in a list
        let mutable count = 0
        for i in list1 do
            if m0.Equals(list1) || m0.Equals(Char.ToLower(list1)) then count + 1 //go thru list and compare to original color (NEED TO ASSIGN THE HEAD A COLOR)
        match check with 
        | (count, 1) -> true 
        | (count, 2) -> 
            if (Char.ToUpper(m0).Equals(m1) || Char.ToUpper(m0).Equals(m2) || Char.ToUpper(m0).Equals(m3) || Char.ToUpper(m0).Equals(m4)) then true //2 is ok if one/two are source squares
            else false
        | (count, 3) -> false
        | (count, 4) -> false









    0 // return an integer exit code
