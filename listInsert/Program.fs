open System // * already in file, don't change

//-------Your functions--------------
let insertElement a idx lst =
    let head, tail = List.splitAt idx lst
    head @ [a] @ tail

let insertElement2 a idx (lst:'a list) =
    let head, tail = lst.[0..idx-1], lst.[idx..lst.Length-1]
    head @ [a] @ tail

let insertList l idx lst =
    let head, tail = List.splitAt idx lst
    head @ l @ tail
   
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let l = [3; 2; 4]
    let a = 3
    let b = 5
    let lst = [1; 2; 3; 4; 5; 6; 7]
    let res = insertElement2 a b lst
    printfn "%A" res
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code