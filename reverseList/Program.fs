open System // * already in file, don't change

//-------Your functions--------------
let reverse lst =
    List.fold (fun lst el -> el::lst) [] lst
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = [1; 2; 3; 4]
    let a' = reverse a
    printfn "%A" a'
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code