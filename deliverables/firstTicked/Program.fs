open System // * already in file, don't change

//-------Your functions--------------
let fact n =
    if n = 0
    then 1
    else List.reduce (*) [1..n]

let sineSeries x n = 
    let term i = 
        let num = -1.0 ** (float i) * (x ** (2.0 * (float i) + 1.0))
        let den = float (fact(2 * i + 1))
        num / den
    if n < 1
    then 0.0
    else [0..n] |> List.map term |> List.reduce (+)

//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let x = 90.0
    let n = 5
    let result = sineSeries x n

    printfn "%A" result
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code