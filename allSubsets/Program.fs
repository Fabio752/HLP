open System // * already in file, don't change

//-------Your functions--------------
let subsetsWithSingleton subsets singleton =
    List.allPairs subsets [[singleton];[]]
    |> List.map (fun (x,y) -> List.append x y)

let subsets lst =
    List.fold subsetsWithSingleton [[]] lst

let rec recSubsets lst =
   match lst with
    | hd::tl -> 
        List.allPairs [[hd];[]] (subsets tl)
        |> List.map (fun (x,y) -> List.append x y)
    | [] -> [[]]
  
let tailRecSubsets lst =
    let rec subsets' l subs =
        match l with
        | hd::tl ->
            List.allPairs subs [[hd];[]]
            |> List.map ((<||) List.append)
            |> subsets' tl
        | [] -> subs
    subsets' lst [[]]
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = [1; 2; 3; 4]
    let subsets = subsets a
    let recSubsets = recSubsets a
    let tailRecSubsets = tailRecSubsets a
    printfn "%A" subsets
    printfn "%A" recSubsets
    printfn "%A" tailRecSubsets
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code