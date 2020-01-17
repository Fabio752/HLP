open System // * already in file, don't change

//-------Your functions--------------
let rec merge l1 l2 =
    match l1, l2 with
    | h1 :: t1, h2 :: t2 when h1 <= h2 ->
        printfn "merging:%A with %A" l1 l2
        printfn "next merged element: %A" h1
        h1 :: merge t1 l2
    | x, [] ->
        x
    | _ ->
        merge l2 l1

let rec sort lst =
    printfn "sorting %A" lst
    match List.length lst with
    | 0 | 1 -> lst
    | n ->
        let l1,l2 = List.splitAt (n / 2) lst
        printfn "Splitting into %A %A" l1 l2
        merge (sort l1) (sort l2)
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = [1; 3; 2; -1]
    let res = sort a
    printfn "%A" res
    
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code