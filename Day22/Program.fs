open System
open System.Collections.Generic
open System.Diagnostics
open System.Linq

type Wind = North | East | South | West

type Turn = Left | Right | Straight | Back

type Coords = int * int

type Infection = Weakened | Flagged | Infected

let turn (wind: Wind) (turn: Turn) : Wind =
    match wind, turn with
    | East, Left | West, Right | South, Back -> North
    | North, Right | South, Left | West, Back -> East
    | North, Left | South, Right | East, Back -> West
    | East, Right | West, Left | North, Back -> South
    | wind, Straight -> wind

let orient (infection: Option<Infection>) : Turn =
    match infection with
    | None -> Left
    | Some(Weakened) -> Straight
    | Some(Flagged) -> Back
    | Some(Infected) -> Right

let touch (infection: Option<Infection>) : Option<Infection> =
    match infection with
    | None -> Some(Weakened)
    | Some(Weakened) -> Some(Infected)
    | Some(Infected) -> Some(Flagged)
    | Some(Flagged) -> None

let go ((x, y): Coords) (wind: Wind) : Coords =
    match wind with
    | North -> (x, y - 1)
    | East -> (x + 1, y)
    | South -> (x, y + 1)
    | West -> (x - 1, y)

let part1 (grid: Set<Coords>) (pos: Coords) (n: int) : int =
    let count = ref 0
    let grid = ref grid
    let pos = ref pos
    let wind = ref North
    for _ in 1..n do
        let infected = Set.contains !pos !grid
        grid := (if infected then Set.remove else Set.add) !pos !grid
        wind := turn !wind (if infected then Right else Left)
        pos := go !pos !wind
        if not infected then incr count
    !count

let part1a (grid: HashSet<Coords>) (pos: Coords) (n: int) : int =
    let count = ref 0
    let pos = ref pos
    let wind = ref North
    for _ in 1..n do
        let infected = grid.Remove !pos
        if not infected then
            incr count
            grid.Add !pos |> ignore
        wind := turn !wind (if infected then Right else Left)
        pos := go !pos !wind
    !count

let read1 (input: String array) =
    seq { 0 .. Array.length input - 1 }
    |> Seq.collect (fun y ->
        let s = input.[y]
        seq { 0 .. s.Length - 1 }
        |> Seq.choose (fun x -> if s.[x] = '#' then Some ((x, y)) else None))

let read2_immutable (input: String array) : Map<Coords, Infection> =
    seq { 0 .. Array.length input - 1 }
    |> Seq.collect (fun y ->
        let s = input.[y]
        seq { 0 .. s.Length - 1 }
        |> Seq.choose (fun x -> if s.[x] = '#' then Some (((x, y), Infected)) else None))
    |> Map.ofSeq

let part2_immutable (grid: Map<Coords, Infection>) (pos: Coords) (n: int) : int =
    let grid = ref grid
    let pos = ref pos
    let wind = ref North
    let count = ref 0
    for _ in 1..n do
        let infected = Map.tryFind !pos !grid
        let touched = touch infected
        grid :=
            !grid |>
            match touched with
            | None -> Map.remove !pos
            | Some(value) ->  Map.add !pos value
        if touched = Some(Infected) then incr count
        wind := orient infected |> turn !wind
        pos := go !pos !wind
    !count

let read2_mutable (input: String array) : Dictionary<Coords, Infection> =
    (
        seq { 0 .. Array.length input - 1 }
        |> Seq.collect (fun y ->
            let s = input.[y]
            seq { 0 .. s.Length - 1 }
            |> Seq.choose (fun x -> if s.[x] = '#' then Some (((x, y), Infected)) else None))
    ).ToDictionary(fst, snd)

let part2_mutable (grid: Dictionary<Coords, Infection>) (pos: Coords) (n: int) : int =
    let pos = ref pos
    let wind = ref North
    let count = ref 0
    for _ in 1..n do
        let infected =
            match grid.TryGetValue !pos with
            | false, _ -> None
            | _, value -> Some(value)
        let touched = touch infected
        match touched with
        | None -> grid.Remove !pos |> ignore
        | Some(value) ->  grid.[!pos] <- value
        if touched = Some(Infected) then incr count
        wind := orient infected |> turn !wind
        pos := go !pos !wind
    !count

let read2_2darray (input: String array) : Infection option [,] =
    let result = Array2D.create 2000 2000 None
    seq { 0 .. Array.length input - 1 }
    |> Seq.collect (fun y ->
        let s = input.[y]
        seq { 0 .. s.Length - 1 }
        |> Seq.choose (fun x -> if s.[x] = '#' then Some ((1000 + x, 1000 + y)) else None))
    |> Seq.iter (fun ((x, y)) -> result.[x, y] <- Some(Infected))
    result

let part2_2darray (grid: Infection option [,]) (pos: Coords) (n: int) : int =
    let pos = ref pos
    let wind = ref North
    let count = ref 0
    for _ in 1..n do
        let (x, y) = !pos
        let infected = grid.[x, y]
        let touched = touch infected
        grid.[x, y] <- touched
        if touched = Some(Infected) then incr count
        wind := orient infected |> turn !wind
        pos := go !pos !wind
    !count

let test_input= [| "..#"; "#.."; "..." |]

let challenge_input =
    [|
        ".###.#.#####.##.#...#...."
        "..####.##.##.#..#.....#.."
        ".#####.........#####..###"
        "#.#..##..#.###.###.#.####"
        ".##.##..#.###.###...#...#"
        "#.####..#.#.##.##...##.##"
        "..#......#...#...#.#....#"
        "###.#.#.##.#.##.######..#"
        "###..##....#...##....#..."
        "###......#..#..###.#...#."
        "#.##..####.##..####...##."
        "###.#.#....######.#.###.."
        ".#.##.##...##.#.#..#...##"
        "######....##..##.######.."
        "##..##.#.####.##.###.#.##"
        "#.###.#.##....#.##..####."
        "#.#......##..####.###.#.."
        "#..###.###...#..#.#.##..."
        "#######..#.....#######..#"
        "##..##..#..#.####..###.#."
        "..#......##...#..##.###.#"
        "....##..#.#.##....#..#.#."
        "..#...#.##....###...###.#"
        "#.#.#.#..##..##..#..#.##."
        "#.####.#......#####.####."
    |]
    
let timed label f =
    let sw = Stopwatch.StartNew()
    f()
    sw.Stop()
    printfn "%s : %d ms elapsed" label sw.ElapsedMilliseconds

[<EntryPoint>]
let main argv =
    let part1s = [|
        "t7", test_input, (1, 1), 7
        "t70", test_input, (1, 1), 70
        "t10k", test_input, (1, 1), 10_000
        "part1", challenge_input, (12, 12), 10_000
    |]
    
    fun () ->
        for label, input, pos, n in part1s do
            part1 (read1 input |> set) pos n |> printfn "%-8s : %d" label
    |> timed "Part 1"
    
    fun () ->
        for label, input, pos, n in part1s do
            part1a (read1 input |> HashSet) pos n |> printfn "%-8s : %d" label
    |> timed "Part 1a"

    let part2s = [|
        "t8", test_input, (1, 1), 8
        "t100", test_input, (1, 1), 100
        "t10M", test_input, (1, 1), 10_000_000
        "part2", challenge_input, (12, 12), 10_000_000
    |]

    fun () ->
        for label, input, pos, n in part2s do
            part2_immutable (read2_immutable input) pos n |> printfn "%-8s : %d" label
    |> timed "Part 2 - immutable"

    fun () ->
        for label, input, pos, n in part2s do
            part2_mutable (read2_mutable input) pos n |> printfn "%-8s : %d" label
    |> timed "Part 2 - mutable"

    fun () ->
        for label, input, (x, y), n in part2s do
            let pos = (x + 1000, y + 1000)
            part2_2darray (read2_2darray input) pos n |> printfn "%-8s : %d" label
    |> timed "Part 2 - 2D array"

    0 // return an integer exit code
