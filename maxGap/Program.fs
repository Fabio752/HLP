open System // * already in file, don't change

//-------Your functions--------------
let maxGap lst =
    lst
    |> List.sort
    |> List.pairwise
    |> List.map (fun (a, b) -> b - a)
    |> List.max

    // ((a >> b) >> c) >> d((a>>b)>>c) = d c(a>>b)= d c b a
let simplerMaxGap =
    List.sort >>
    List.pairwise >>
    List.map (fun (a, b) -> b - a) >>
    List.max 
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = [1; 2; 3; 4; -1; 10; 22; -30]
    let b = simplerMaxGap a
    printfn "%A" b
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code