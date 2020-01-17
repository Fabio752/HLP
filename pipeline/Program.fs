open System // * already in file, don't change

//-------Your functions--------------
let printPipe x = printfn "%A" x; x

let testPipe x =
    x
    |> List.pairwise
    |> printPipe // A
    |> List.indexed
    |> List.filter (fun (i, _) -> i % 2 <> 0)
    |> printPipe // B
    |> List.map (fun (_, b) -> fst b + snd b) 
    |> printPipe // C
    |> List.sum
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let x = [1; 2; 3; 4; -1]
    let y =  testPipe x
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code