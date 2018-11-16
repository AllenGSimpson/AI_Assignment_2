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
    
/////////////////////////////EVERYTHING ABOVE THIS LINE IS FOR WRITING PRETTY THINGS TO CONSOLE **IGNORE**////////////////////////////////////

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
    
    //FOR DEBUGGING
    let readMazeFromString str = 
        let lines = (str:string).Split('\n','\r') |> Seq.map (fun s -> s.Trim()) |> Seq.filter (System.String.IsNullOrEmpty >> not) |> Seq.toArray
        { 0 .. lines.Length-1}
        |> Seq.collect (fun y ->
            lines.[y]
            |> Seq.mapi (fun x c -> (x,y), c)
        )
        |> Map.ofSeq
    //definitions...
    type Maze = Map<int*int,char>
    type Head = {
        x : int
        y : int
        color : char
    }

    type MazeState = 
        {
            maze  : Maze
            heads : List<Head>
            starts: Map<char,int*int>
            goals: Map<char,int*int>
        }
    //Defintitons above^^^
    //create a mazeState from a maze.
    let createMazeState (maze:Maze) =
        let mutable starts = Map.empty
        let mutable goals = Map.empty
        maze
        |> Seq.sortBy (function (KeyValue((x,y),_)) -> y,x )    //flip the maze around
        |> Seq.map (function (KeyValue(p,color)) -> p,color)    //unwrapping the map KeyValuePair values
        |> Seq.filter (function _,'_' -> false | _,'.' -> false | _ -> true)     //filter out blank spots
        |> Seq.iter (fun (p,color) ->
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
    
    //Check that for all remaining heads, there is a goal state that has a '_' around it (or has access) this is a redundant check, but removing it increases states so we're leaving it in.
    let hasAccess ({maze=maze';heads=heads;starts=starts;goals=goals} as mazeState:MazeState) ({color=color;x=x;y=y},isGoal) = 
        let maze = maze'.Add((x,y),color) // stick color in the correct spot for this move
        if heads.Length = 0 then false
        else
            heads
            |>Seq.forall(fun h ->                 
                let goalx,goaly = goals.[h.color]
                match maze.TryFind(goalx-1,goaly),maze.TryFind(goalx,goaly-1),maze.TryFind(goalx+1,goaly),maze.TryFind(goalx,goaly+1) with
                | Some c, _, _, _ when c = '_' || c = h.color -> true
                | _, Some c, _, _ when c = '_' || c = h.color -> true
                | _, _, Some c, _ when c = '_' || c = h.color -> true   
                | _, _, _, Some c when c = '_' || c = h.color -> true
                | _ -> false
            )
            

    //get the moves that are valid for this head and this maze state
    let getValidMoves ({maze=maze;starts=starts;goals=goals} as mazeState:MazeState) (head : Head) =
        let sShaped (x,y) = //This is how we define an S shape 
            let allColor (pa,pb,pc) = 
                let goalPoint = goals.[head.color]
                match maze.TryFind pa, maze.TryFind pb, maze.TryFind pc with 
                | Some a, Some b, Some c when a = b && b = c && a = head.color -> 
                    not(pa = goalPoint || pb=goalPoint || pc = goalPoint)
                | _ -> false

            let upperLeft = ((x-1,y-1),(x,y-1),(x-1,y)) |> allColor             // in short this:
            let upperRight = ((x,y-1),(x+1,y-1),(x+1,y)) |> allColor            //  hh
            let lowerRight = ((x+1,y),(x+1,y+1),(x,y+1)) |> allColor            //  hH
            let lowerLeft = ((x,y+1),(x-1,y+1),(x-1,y)) |> allColor             //  where H is the head and h are the same color as the head, just rotated.

            upperLeft || upperRight || lowerLeft || lowerRight
            
        let rec loop directions cont = // we will look at a list of points (directions) and a function (cont)
            match directions with   // Directions are..
            | [] -> cont []         // Empty, so call cont on an empty list.
            | (x,y) as dir :: rest ->   // a point (we will cal dir) and the rest of the list.
                if goals.[head.color] = dir then loop rest (fun xs -> ({color = head.color; x = x; y = y},true) :: xs |> cont)      // if this is the goal then stop looking for anything else and just return the goal
                elif (maze.[dir] <> '_') || (sShaped dir) then loop rest cont                                                       //if the position is not a '_' or will create and S, continue the loop
                else loop rest (fun xs -> ({color = head.color; x = x; y = y},false) :: xs |> cont)                                 //otherwise loop on the rest of the list with the head (from the getValidMoves parameter) added and the cont fn
                
        let directions =
            [
                (head.x-1,head.y) // west
                (head.x,head.y-1) // north
                (head.x+1,head.y) // east
                (head.x,head.y+1) // south
            ]
            |> List.filter (fun p -> maze.ContainsKey p)    // remove any locations outside the maze

        loop directions id  //loop with directions (as defined above) and an identity function that takes a value and returns it. 
                            //(therefore, the [] on line 164 (matching directions the first time) will retun an [])
    
    //get the distance between a and b (for find path)
    let dist (ax,ay) (bx,by) = 
        let dx = float(bx-ax) 
        let dy = float(by-ay)
        sqrt(dx*dx+dy*dy)

    //This is a greedy first search implemented in F#, all it's for is finding if you can make a path from start to goal, it's ugly I know but not worth explaining in detail
    //It is similar to the find frontier though...
    let findPath start goal (maze:Maze) =
        let rec loop (x,y) (maze:Maze) backtrack =
            let directions =
                [
                    -1,0 // west
                    0,-1 // north
                    1,0// east
                    0,1// south
                ]
                |> Seq.map (fun (a,b) -> a+x,b+y)
                |> Seq.choose (fun p -> if p = goal then Some (p,0.) else maze.TryFind p |> Option.filter (fun c -> c = '_') |> Option.map (fun _ -> p,dist p goal))
                |> Seq.sortBy snd
                |> Seq.toList
            match directions with
            | [] -> backtrack maze
            | (h,_) :: _ when h = goal -> true
            | (h,_) :: rest ->
                loop h (maze.Add(h,'.')) (fun maze' -> loop2 rest maze' backtrack)
        and loop2 otherDirs maze backtrack =
            match otherDirs with
            | [] -> backtrack maze
            | (h,_) :: _ when h = goal -> true
            | (h,_) :: rest when maze.TryFind h = Some '_' ->
                loop h (maze.Add(h,'.')) (fun maze' -> loop2 rest maze' backtrack)
            | _ :: rest -> loop2 rest maze backtrack
        loop start maze (fun _ -> false)
    
    //find the frontier of a point as a list
    let findFrontier pt (maze:Maze) =
        let rec loop (x,y) (maze:Maze) frontier edge =  //recusive check
            let directions,newEdges =   //get the directions
                [
                    -1,0 // west
                    0,-1 // north
                    1,0// east
                    0,1// south
                ]
                |> List.map(fun (a,b) -> a+x,b+y) //get the directions as x,y points around (x,y)
                |> List.choose(fun p -> maze.TryFind p |> Option.filter (fun c -> c <> '.') |> Option.map (fun c -> p,c))   //if it a direction is a color is not '.', it maps that point, with it's color to..
                |> List.partition (fun (p,c) -> c = '_')   // here where it splits the list of directions into spots where the directions color is '_' and not '_'. 
                |> fun (a,b) -> a|>List.map fst,b|>List.map fst // here we take the two lists, (a and b here) and grab the first element of a, and the first of b and send them off as the result.
            
            loop2 (directions@frontier) (directions |> Seq.fold (fun (m:Maze) p -> m.Add(p,'.')) maze) (newEdges@edge)  //this very dense line takes the frontier (from loop's arguements), 
                                                                                                                        //gets the directions around *it*, and  gets the directions, 
                                                                                                                        //with each one mapped a '.' into thier color, and gets the edges of 
                                                                                                                        //the frontier and sends them all to loop2
        and loop2 frontier (maze:Maze) edge =   //LOOP2
            match frontier with //here we look at the frontier (an int*int list)
            | [] -> edge,maze   //if it's an empty list return us the edge and the maze given as args.
            | h :: rest ->      //else get the head of the list (an int*int)...
                loop h maze rest edge   //and call loop on it (with the rest of the list being the frontier)...
        
        loop pt (maze.Add(pt,'.')) [] [] //we start finding the frontier by adding a '.' at the start point and giving it an empty list for the frontier so far and the edge of the frontier.

    let checkIfSpaceRegionsAreOk (mazeState:MazeState) =
        let maze = mazeState.maze   //we grab the maze out of the mazeState
        //here we grab the head and the goal points
        let activeHeadsAndGoals = mazeState.heads |> Seq.fold (fun (s:Set<_>) head -> let p = head.x,head.y in s.Add(p).Add(mazeState.goals.[head.color])) Set.empty 
        let rec loop (maze:Maze) = //and we call a loop on them
            match maze |> Seq.tryFind (function | KeyValue (_,'_') -> true | _ -> false) with // we search our maze for a point where the color is '_'
            | None -> true      //if we find no such point return good
            | Some (KeyValue(p,_)) -> //else grab the point and...
                // check this space region for any heads or open goals
                let edges,maze' = findFrontier p maze
                if edges |> Seq.exists activeHeadsAndGoals.Contains then
                    loop maze' //call loop on the new maze (with the frontier added)
                else false
        loop maze //start the loop
                    
    let rec runMazeStateForward (mazeState : MazeState) = 

        let updateHead ({color = color; x=x ; y=y},isGoal) =    //add the head to the maze and, if it is a goal, remove it from the mazeStates.heads list.
            {
                mazeState with
                    maze = mazeState.maze.Add((x,y),color)
                    heads = 
                        if isGoal then  
                            mazeState.heads |> List.filter (fun h -> h.color <> color)
                        else
                            mazeState.heads |> List.map (fun h -> if h.color = color then {h with x=x;y=y} else h) //changing the xy coordinates
            }

        let allTheHeadsHavePaths =      //search for a path from the head to it's respective goal state, return true if all heads have paths
            let maze = mazeState.maze
            mazeState.heads |> Seq.forall (fun head -> findPath (head.x,head.y) (mazeState.goals.[head.color]) maze)

        if checkIfSpaceRegionsAreOk mazeState |> not then None // if the edges around any of the space regions don't have a active head or goal then this is a bad state and backtrack
        else
            if allTheHeadsHavePaths then                    //check to make sure all the heads in the list have a path to the goal
                //let f = getValidMoves mazeState           //Partially applied version of line 107
                mazeState.heads  
                |> List.map (getValidMoves mazeState)       // same as List.map (fun x -> getValidMoves mazeState x)
                |> List.sortBy (fun x -> x.Length)          // first element should be the smallest list of heads with directions to go to
                |> function
                    | [] -> Some [mazeState]                //there are no heads left (remove heads from list when they are complete) that need to move so the mazestate is finished 
                    | [] :: _ -> None                       //backtrack (Redundant case - this should never be hit, however if it does hit this case, backtrack)
                    | [(_,_) as head]  :: t  -> head|> updateHead |> runMazeStateForward  //during a goalcase we need to place the new location in the maze and remove the head color
                    | h::t -> 
                        h
                        |> List.map (updateHead)            //update each valid head in h.
                        |> Some                             //return a list of possible MazeState choices
            else None


    //This will not beat anything bigger than a 4x4 in less than 3 hours (unless the possible paths are extremely constrained.
    let rec runMazeStateSimple (mazeState : MazeState) = 
        //same thing from upthere
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
        mazeState.heads  
        |> List.collect (getValidMoves mazeState)       // same as List.map (fun x -> getValidMoves mazeState x)
        |> function                             //look at the valid moves
            | [] -> None                        // if there are none, backtrack 
            | hs ->                             //else..
                hs                              //take those heads
                |> List.map (updateHead)        //update the heads on the mazeState for each
                |> Some                         //return a list of possible MazeState choices
            
    //Just for displaying!
    let printMazeState (mazeState:MazeState) =
        let xmin,xmax,ymin,ymax = mazeState.maze |> Seq.map (function | KeyValue(p,_) -> p) |> Seq.fold (fun (xmin,xmax,ymin,ymax) (x,y) -> (min xmin x,max xmax x,min ymin y,max ymax y)) (System.Int32.MaxValue,System.Int32.MinValue,System.Int32.MaxValue,System.Int32.MinValue)
        let xrange, yrange = xmax-xmin, ymax-ymin
        for y in 0 .. yrange do
            for x in 0 .. xrange do
                let p = (xmin+x,ymin+y)
                match mazeState.maze.TryFind p with
                | None -> printf " "
                | Some '_' -> printf "_"
                | Some '.' -> printf "."
                | Some c ->
                    if mazeState.goals.[c] = p || mazeState.starts.[c] = p then printf "%c" c
                    else System.Char.ToLower(c) |> printf "%c"
            printfn ""
        printfn ""
    //Just for displaying
    let printMazeStates (mazeStates:MazeState list) =
        match mazeStates with
        | [] -> printfn "[]"
        | mazeState::_ ->
            let xmin,xmax,ymin,ymax = mazeState.maze |> Seq.map (function | KeyValue(p,_) -> p) |> Seq.fold (fun (xmin,xmax,ymin,ymax) (x,y) -> (min xmin x,max xmax x,min ymin y,max ymax y)) (System.Int32.MaxValue,System.Int32.MinValue,System.Int32.MaxValue,System.Int32.MinValue)
            let xrange, yrange = xmax-xmin, ymax-ymin
            for y in 0 .. yrange do
                for i in 0 .. mazeStates.Length-1 do
                    let mazeState = mazeStates.[i]
                    for x in 0 .. xrange do
                        let p = (xmin+x,ymin+y)
                        match mazeState.maze.TryFind p with
                        | None -> printf " "
                        | Some '_' -> printf "_"
                        | Some '.' -> printf "."
                        | Some c ->
                            let bkc = System.Console.BackgroundColor
                            if mazeState.starts.[c] = p then
                                System.Console.BackgroundColor <- System.ConsoleColor.Cyan
                                printf "%c" c
                            elif mazeState.goals.[c] = p && mazeState.heads |> List.exists (fun {color=color} -> color = c) then
                                System.Console.BackgroundColor <- System.ConsoleColor.DarkMagenta
                                printf "%c" c
                            elif mazeState.goals.[c] = p then
                                System.Console.BackgroundColor <- System.ConsoleColor.DarkGreen
                                printf "%c" c
                            else System.Char.ToLower(c) |> printf "%c"
                            System.Console.BackgroundColor <- bkc
                    printf " "
                printfn ""
            printfn ""
    //make sure the maze has no '_' values
    let noEmpty mazeState =
        mazeState
        |> Map.exists ( fun _ v -> v = '_' )
        |> not 
    

    let search runMazeState (mazeState:MazeState) = //search function takes a function called runMazeState (one of the two options above) and a mazestate
        let startTime = System.DateTime.Now // get the search's start time
        let mutable count = 0               // init the count
        let rec loop mazeStates backtrackFun =   
            count <- count+1 //increment the number of steps
            
            ////for debugging
            //if count % 1000 = 0 then printfn "\ncount: %d | time: %s \n" count ((System.DateTime.Now - startTime).ToString ())
            //if count % 1000 = 0 then printfn "Set of mazeStates: ";printMazeStates mazeStates;
            //System.Threading.Thread.Sleep(10000)
            //System.Console.ReadKey() |>ignore

            match mazeStates with       //look at the mazestates
            | [] ->                     //if there are no states..
                backtrackFun ()         //backtrack
            | h :: _ when h.heads.Length = 0 && noEmpty h.maze ->   // if we found a state and the heads.length is 0 (and noEmpty returns true on the maze)...
                printfn "\nFound Goal!";                            // we're done
                printfn "count: %d | time: %s \n" count ((System.DateTime.Now - startTime).ToString ());
                Some (h,count) //Only one return value when there are no heads -- goal state 
            | h :: rest when h.heads.Length = 0 ->                  // IF NOEMPTY FAILED and we also have no heads we need to backtrack becasue we have a space in the maze...
                loop rest backtrackFun
            | h :: rest ->                                          // OTHERWISE...
                match runMazeState h with                           //we call the runMazeState function on the head of the mazeStates list and match the result with../
                | None ->                                               //we found nothing that worked...
                    loop rest backtrackFun // didn't work, to try next
                | Some rest' ->                                         //we found something that worked! 
                    loop rest' (fun _ -> loop rest backtrackFun) // So we put the rest on a stack and start working on the new possiblities
        
        loop [mazeState] (fun _ -> None) //here we begin the loop!
    



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

    time 10000 (fun _ -> m1.[1,1] |> ignore)
    time 10000 (fun _ -> m2.[1,1] |> ignore)
    time 10000 (fun _ -> m3.[9,5] |> ignore)

    time 10000 (fun _ -> m1 |> Map.add (1,1) 'B' |> ignore)
    time 10000 (fun _ -> let m' : char[,] = m2.Clone() |> unbox in m'.[1,1] <- 'B'; m' |> ignore)
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