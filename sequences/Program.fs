open System // * already in file, don't change

//-------Your functions--------------
let sequences a b =
    List.allPairs [a..b] [a..b]
    |> List.filter (fun (a, b) -> a <=b )
    |> List.map (fun (a, b) -> [a..b])

let sequences2 a b =
    List.allPairs [a..b] [a..b]
    |> List.collect (fun (a, b) -> if a <= b then [[a..b]] else [])

//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = 3
    let b = 5
    let c = sequences2 a b
    printfn "%A" c
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code