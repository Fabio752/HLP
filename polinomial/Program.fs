open System // * already in file, don't change

//-------Your functions--------------


let poly coeffs x = 
    let term n an = 
        an * (x ** (float n))
    List.mapi term coeffs |> List.reduce (+)

//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    //let points = [0.0..1.0]
    let x = 2.0
    let coeffs = [1.0; 0.5; 0.0; 0.25]  //[a_0;a_1;a_2;a_3]  
    let result = poly coeffs x
    // let samples = List.map poly coeffs points
    printfn "%A" result
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code