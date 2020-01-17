open System // * already in file, don't change

//-------Your functions--------------
let tabulate (lstOfRows: string list list) =
    // calculate max number of Columns
    let maxCols =
            lstOfRows
            |> List.map List.length
            |> List.max
    // Deal with None values
    let leftAlignRow lst =
        [0..maxCols-1]
        |> List.map (fun n -> List.tryItem n lst |> function | None -> "" | Some s -> s)
    let lstOfRows' = List.map leftAlignRow lstOfRows
    // calculate max width of each column
    let colWidths =
        List.transpose lstOfRows'
        |> List.map (List.map String.length >> List.max)

    // pad each row
    let padRow row =
        List.zip colWidths row
        |> List.map (fun (w,s:string)-> s.PadRight w)
    
    lstOfRows'
    |> List.map padRow
    
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    let a = [
        ["aa" ; "abc"]
        ["abcde"] 
        ["abc"; "a"; "axxx"]
    ]   
    let res = tabulate a
    printfn "%A" res
    
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code