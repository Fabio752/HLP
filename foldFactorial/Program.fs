open System // * already in file, don't change

//-------Your functions--------------
let fact n =
    [1..n] |> List.fold (*) 1

let greaterThan0 a =
    a > 0 
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = fact 4
    printfn "%A" a
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code