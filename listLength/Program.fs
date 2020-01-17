open System // * already in file, don't change

//-------Your functions--------------
let listLength lst =
    (0, lst) ||> List.fold(fun len _ -> len + 1)

//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = ["a"; "a"; "a"; "a"; "a"; "a"]
    let result = listLength a
    printfn "%A" result
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code