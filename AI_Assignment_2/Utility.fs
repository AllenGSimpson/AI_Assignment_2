 module Utility 

    module VTerm =
        open System.Runtime.InteropServices
        open System

        [<Flags>]
        type ConsoleMode =
            | ENABLE_ECHO_INPUT = 0x0004u
            | ENABLE_EXTENDED_FLAGS = 0x0080u
            | ENABLE_INSERT_MODE = 0x0020u
            | ENABLE_LINE_INPUT = 0x0002u
            | ENABLE_MOUSE_INPUT = 0x0010u
            | ENABLE_PROCESSED_INPUT = 0x0001u
            | ENABLE_QUICK_EDIT_MODE = 0x0040u
            | ENABLE_WINDOW_INPUT = 0x0008u
            | ENABLE_VIRTUAL_TERMINAL_INPUT = 0x0200u  // allows all the VT100 goodness, this is also how all the NEW goodys in Windows 10 Console are surfaced
            | ENABLE_PROCESSED_OUTPUT = 0x0001u
            | ENABLE_WRAP_AT_EOL_OUTPUT = 0x0002u
            | ENABLE_VIRTUAL_TERMINAL_PROCESSING = 0x0004u
            | DISABLE_NEWLINE_AUTO_RETURN = 0x0008u    // If you're extensively using VT100 then you might need to use this mode as well.
            | ENABLE_LVB_GRID_WORLDWIDE = 0x0010u

        type StandardHandles =
            | STD_INPUT_HANDLE = -10
            | STD_OUTPUT_HANDLE = -11
            | STD_ERROR_HANDLE = -12

        [<DllImport("kernel32",CallingConvention=CallingConvention.Winapi,SetLastError=true)>]
        extern bool GetConsoleMode(System.IntPtr hConsoleHandle,ConsoleMode* mode);

        [<DllImport("kernel32",CallingConvention=CallingConvention.Winapi,SetLastError=true)>]
        extern bool SetConsoleMode(System.IntPtr hConsoleHandle,ConsoleMode mode);

        [<DllImport("kernel32",CallingConvention=CallingConvention.Winapi,SetLastError=true)>]
        extern IntPtr GetStdHandle(StandardHandles stdHandle);

        let turnOnVtermSupport() =
            let stdout = GetStdHandle(StandardHandles.STD_OUTPUT_HANDLE)
            let mutable originalConsoleMode = Unchecked.defaultof<_>
            if GetConsoleMode(stdout,&&originalConsoleMode) |> not then
                Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error())
            if SetConsoleMode(stdout,originalConsoleMode|||ConsoleMode.ENABLE_VIRTUAL_TERMINAL_PROCESSING) |> not then
                Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error())
            System.Console.OutputEncoding <- System.Text.Encoding.UTF8
            originalConsoleMode
        
    do
        try
            VTerm.turnOnVtermSupport() |> ignore
        with
            | e -> printfn "Can't turn on VTerm mode, perhaps we're running in a redirected console?\n%s" e.Message
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

    //let readMazeAsDictionary file = 
    //    let lines = System.IO.File.ReadAllLines(file)
    //    { 0 .. lines.Length-1}
    //    |> Seq.collect (fun y ->
    //        lines.[y]
    //        |> Seq.mapi (fun x c -> (x,y), c)
    //    )
    //    |> Seq.fold (fun (d:System.Collections.Generic.Dictionary<_,_>) (k,v) -> d.Add(k,v); d) (System.Collections.Generic.Dictionary())


    /// <summary>Reads a text based maze from the given file into a 2D char array with x as first index and and y as second index (Column First Order).</summary>
    /// <param name="file">the filename of the maze file.</param>
    //let readMazeAsArray file = 
    //    let lines = System.IO.File.ReadAllLines(file)
    //    Array2D.init (lines.[0].Length) (lines.Length) (fun x y -> lines.[x].[y])
        
    /// <summary>Allows you to calculate the time cost of any function given the number of repeats and the function.</summary>
    /// <param name="repCnt">the number of repeats.</param>

    type Maze = Map<int*int,char> //Entire maze/game (n)
    type Head = {                 //unconnected path "ends" (n) 
        x : int
        y : int
        color : char
    }

    type MazeState =             //Maze at different states (n)
        {
            maze  : Maze
            heads : List<Head>
            starts: Map<char,int*int>
            goals: Map<char,int*int>
        }
    
    let createMazeState (maze:Maze) =
        let mutable starts = Map.empty
        let mutable goals = Map.empty
        maze
        |> Seq.sortBy (function (KeyValue((x,y),_)) -> y,x )    //flip the maze around
        |> Seq.map (function (KeyValue(p,color)) -> p,color)    //unwrapping the map KeyValuePair values
        |> Seq.filter (function _,'_' -> false | _ -> true)     //filter out blank spots
        |> Seq.iter (fun (p,color) ->                           //one same-color source block is marked start, the other same-color is marked goal (n)
            if starts.ContainsKey color |> not then
                starts <- Map.add color p starts
            elif goals.ContainsKey color |> not then
                goals <- Map.add color p goals
            )
        // starts have all the starts and goals have all the goals
        {
            maze = maze
            heads = starts |> Seq.map (function (KeyValue(color,(x,y))) -> {x = x; y = y; color = color}) |> Seq.toList // compute the heads list from our initial starts map.
            starts = starts
            goals = goals
        }

    // let starts, goals, head = mazeState.starts, mazeState.goals, mazeState.heads.[0]

    let getValidMoves ({maze=maze;starts=starts;goals=goals} as mazeState:MazeState) (head : Head)=
        let sShaped (x,y) =         //constraint 1: no zig-zags allowed (n)
            let allColor (pa,pb,pc) = 
                match maze.TryFind pa, maze.TryFind pb, maze.TryFind pc with 
                | Some a, Some b, Some c when a = b && b = c && a = head.color -> true
                | _ -> false

            let upperLeft = ((x-1,y-1),(x,y-1),(x-1,y)) |> allColor 
            let upperRight = ((x,y-1),(x+1,y-1),(x+1,y)) |> allColor
            let lowerRight = ((x+1,y),(x+1,y+1),(x,y+1)) |> allColor
            let lowerLeft = ((x,y+1),(x-1,y+1),(x-1,y)) |> allColor

            upperLeft || upperRight || lowerLeft || lowerRight //if any of these are TRUE, then zig-zag exists (n)
            
        let rec loop directions cont =
            match directions with
            | [] -> cont []
            | (x,y) as dir :: rest ->
                if goals.[head.color] = dir then [{color = head.color; x = x; y = y},true]      // if this is the goal then stop looking for anything else and just return the goal
                elif (maze.[dir] <> '_') || (sShaped dir) then loop rest cont                   //if the position is not a '_' or will create and S, continue the loop (constraint 2 (n))
                else loop rest (fun xs -> ({color = head.color; x = x; y = y},false) :: xs |> cont)
                
        let directions =
            [
                (head.x-1,head.y) // west
                (head.x,head.y-1) // north
                (head.x+1,head.y) // east
                (head.x,head.y+1) // south
            ]
            |> List.filter (fun p -> maze.ContainsKey p)    //remove any locations outside the maze (constraint 3 (n))
        loop directions id


    let rec runMazeStateForward (mazeState : MazeState) = //forward-looking algorithm
        
        let updateHead {color = color; x=x ; y=y}=
            {
                mazeState with
                    maze = mazeState.maze.Add((x,y),color)
                    heads = mazeState.heads |> List.map (fun h -> if h.color = color then {h with x=x;y=y} else h) //changing the xy coordinates
            }
        
        //let f = getValidMoves mazeState           //Partially applied version of line 107
        mazeState.heads  
        |> List.map (getValidMoves mazeState)       // same as List.map (fun x -> getValidMoves mazeState x)
        |> List.sortBy (fun x -> x.Length)          // first element should be the smallest list of heads with directions to go to (most constrained path (n))
        |> function
            | [] -> Some [mazeState]                //there are no heads left (remove heads from list when they are complete) that need to move so the mazestate is finished (solution? (n))
            | [] :: _ -> None                       //backtrack because there's an uncompleted connection with no available paths (n)
            | [{color = color; x=x ; y=y},true]  :: t  -> 
                //during a goalcase we need to place the new location in the maze and remove the head color
                {
                    mazeState with 
                        maze = mazeState.maze.Add((x,y),color)
                        heads = mazeState.heads |> List.filter (fun h -> h.color <> color)  //removing head because we found the goal
                } |> runMazeStateForward 

            | [h,false]  :: t  -> h |> updateHead |> runMazeStateForward                    //when not a goalcase we need to place the new location in the maze and try again
            | h::t -> 
                h
                |> List.map (fst >> updateHead)
                |> Some                             //return a list of possible MazeState choices
    
    let rec runMazeStateSimple (mazeState : MazeState) = //non-forward-looking (simple) algorithm
        
        let updateHead ({color = color; x=x ; y=y},isGoal)=
            {
                mazeState with
                    maze = mazeState.maze.Add((x,y),color)
                    heads = 
                        if isGoal then  
                            mazeState.heads |> List.filter (fun h -> h.color <> color)
                        else
                            mazeState.heads |> List.map (fun h -> if h.color = color then {h with x=x;y=y} else h) //changing the xy coordinates
            }
        //let f = getValidMoves mazeState           //Partially applied version of line 107
        mazeState.heads  
        |> List.collect (getValidMoves mazeState)       // same as List.map (fun x -> getValidMoves mazeState x)
        |> function
            | [] -> None                //backtrack 
            | hs -> 
                hs
                |> List.map (updateHead)
                |> Some                             //return a list of possible MazeState choices

    let printMazeState (mazeState:MazeState) = //print function (n)
        let xmin,xmax,ymin,ymax = mazeState.maze |> Seq.map (function | KeyValue(p,_) -> p) |> Seq.fold (fun (xmin,xmax,ymin,ymax) (x,y) -> (min xmin x,max xmax x,min ymin y,max ymax y)) (System.Int32.MaxValue,System.Int32.MinValue,System.Int32.MaxValue,System.Int32.MinValue)
        let xrange, yrange = xmax-xmin, ymax-ymin
        for y in 0 .. yrange do
            for x in 0 .. xrange do
                let p = (xmin+x,ymin+y)
                match mazeState.maze.TryFind p with
                | None -> printf " "
                | Some '_' -> printf "_"
                | Some c ->
                    if mazeState.goals.[c] = p || mazeState.starts.[c] = p then printf "%c" c
                    else System.Char.ToLower(c) |> printf "%c"
            printfn ""
        printfn ""

    let noEmpty mazeState =
        mazeState
        |> Map.exists ( fun _ v -> v = '_' )
        |> not 
    
    let search runMazeState (mazeState:MazeState) = //executes the appropriate search-algorithm (n)
        let rec loop mazeStates backtrackFun =    
            printMazeState mazeState
            match mazeStates with
            | [] -> printfn "no valid options! backtracking!"; backtrackFun ()
            | h :: _ when h.heads.Length = 0 && noEmpty h.maze -> printfn "Found Goal!"; Some h //Only one return value when there are no heads -- goal state 
            | h :: rest -> 
                match runMazeState h with
                | None -> printfn "doesn't work! backtracking!"; loop rest backtrackFun // didn't work, to try next
                | Some rest -> printfn "bad map! backtracking!"; loop rest (fun _ -> loop rest backtrackFun) //put the rest on a stack and start working on the new possiblities
        loop [mazeState] (fun _ -> None)
    



    //// moves A out and then show two possible positions for P
    //runMazeStateForward mazeState
    //|> Option.get
    //|> List.iter printMazeState

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