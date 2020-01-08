open System // * already in file, don't change

//-------Your functions--------------
let fact n =
    if n = 0
    then 1
    else List.reduce (*) [1..n]

let expo x n =
    let term i =                             // Notice x is no longer argument
        (x ** (float i)) / (float (fact i))  // of term. This is because x is
    [0..n] |> List.map term |> List.reduce (+)
                                             // already an argument of expo
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = expo 3.0 3
    printfn "%A" a
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code