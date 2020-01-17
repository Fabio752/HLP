open System // * already in file, don't change

//-------Your functions--------------
let modalElement lst =
    let counts = 
        lst
        |> List.countBy id
        |> List.sortByDescending snd
    match counts with
    | (_, cnt) ::_ ->
        let modes = List.takeWhile (fun (_, cnt') -> cnt' = cnt) counts
        List.map fst modes, cnt
    | [] -> failwithf "Cant find mode of empty list!"


//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = [1; 1; 1; 2; 2; 3; 3; 4; 4; 4; -1; 10; 22; -30]
    let b = modalElement a
    printfn "%A" b
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code