 module Utility 

    //the following lines allow for comments to show in intellisense
    /// <summary>Reads a text based maze from the given file into a immutable Map of x,y coordinates to char.</summary>
    /// <param name="file">the filename of the maze file.</param>
    let readMaze file = 
        let lines = System.IO.File.ReadAllLines(file)
        { 0 .. lines.Length-1}
        |> Seq.collect (fun y ->
            lines.[y]
            |> Seq.mapi (fun x c -> (x,y), c)
        )
        |> Map.ofSeq

    let readMazeAsDictionary file = 
        let lines = System.IO.File.ReadAllLines(file)
        { 0 .. lines.Length-1}
        |> Seq.collect (fun y ->
            lines.[y]
            |> Seq.mapi (fun x c -> (x,y), c)
        )
        |> Seq.fold (fun (d:System.Collections.Generic.Dictionary<_,_>) (k,v) -> d.Add(k,v); d) (System.Collections.Generic.Dictionary())


    /// <summary>Reads a text based maze from the given file into a 2D char array with x as first index and and y as second index (Column First Order).</summary>
    /// <param name="file">the filename of the maze file.</param>
    let readMazeAsArray file = 
        let lines = System.IO.File.ReadAllLines(file)
        Array2D.init (lines.[0].Length) (lines.Length) (fun x y -> lines.[x].[y])
        
    /// <summary>Allows you to calculate the time cost of any function given the number of repeats and the function.</summary>
    /// <param name="repCnt">the number of repeats.</param>
    let time repCnt (f:unit -> unit) =      //rep allows you to repeat runs to test time cost
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        for x = 0 to repCnt-1 do
            f()
        sw.Stop()
        let cycleTime = sw.Elapsed.TotalSeconds/(float repCnt)*1_000_000_000.0 // get average running time and convert to nanoseconds per loop
        cycleTime
    

    //The following is not to be included in final version, 
    //used for testing concepts only and will be removed for final version
    (* for debugging *
    time 10000 (fun _ -> ())

    let m1 = readMaze file
    let m2 = readMazeAsArray file
    let m3 = readMazeAsDictionary file

    time 10000 (fun _ -> m1.[9,5] |> ignore)
    time 10000 (fun _ -> m2.[9,5] |> ignore)
    time 10000 (fun _ -> m3.[9,5] |> ignore)

    time 10000 (fun _ -> m1 |> Map.add (9,5) 'B' |> ignore)
    time 10000 (fun _ -> let m' : char[,] = m2.Clone() |> unbox in m'.[9,5] <- 'B'; m' |> ignore)
    time 10000 (fun _ -> let m' : System.Collections.Generic.Dictionary<(int*int),char> = System.Collections.Generic.Dictionary(m3) |> unbox in m'.[(9,5)] <- 'B'; m' |> ignore)

    //let modifyMap (m:'a) (k:'key) (v:'value)
    //This allows for generification 

    let myRoutine (modifyMap:'a->'k->'v->'a) (m:'a) =
        modifyMap m (9,5) 'B'

    let modifyMap1 (m:Map<_,_>) k v = Map.add k v m
    let modifyMap2 (m:char[,]) (x,y) v =
        let m' : char[,] = m.Clone() |> unbox
        m'.[x,y] <- v
        m'
    let modifyMap3 (m:System.Collections.Generic.Dictionary<_,_>) k v =
        let m' : System.Collections.Generic.Dictionary<(int*int),char> = System.Collections.Generic.Dictionary(m3)
        m'.[(9,5)] <- 'B'
        m'


    // This is called Statically Resolved Type Parameters see: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/statically-resolved-type-parameters    

    let inline foo< ^T when ^T :(static member Parse: string -> 'T) > (s:string) = (^T : (static member Parse: string -> ^T)(s))
    let i : int = foo "5"
    let f : float = foo "5.6"
    let f : float = foo "5"
    let b : bool = foo "true"

    let inline name< ^T when ^T:(member Name: string) > (o:^T) = (^T:(member Name: string)(o))

    type Bob =
        {
            Name : string
        }

    let b = { Name = "John" }    

    name b

    let bar() = foo()


    *)              