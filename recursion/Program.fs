open System // * already in file, don't change

//-------Your functions--------------
let rec filter f lst =
    match lst with
    | hd::tl when f hd -> hd::(filter f tl)
    | _::tl -> filter f tl
    | [] -> []

let greaterThan0 a =
    a > 0 
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = [1; 2; 3; 4; -1]
    let b = filter greaterThan0 a 
    printfn "%A" a
    printfn "%A" b
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code