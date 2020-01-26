// Learn more about F# at http://fsharp.org

open System // * already in file, don't change

//-------Your functions--------------
/// record type definition with fields Age, Name, Email
/// line breaks are typical, but could be all on one line
type Person = {
        Age: int; 
        FirstName: string; 
        FamilyName: string; 
        Email: string
    }

/// constructing a data item of Person type
let tom = {
    Age=61; 
    FirstName="Thomas"; 
    FamilyName="Clarke"; 
    Email="tom@xx.co.uk"}

/// using copy and update to make a new item
let tomsTwin = {tom with FirstName="Bob"; Email="bob@yy.co.uk"}

/// access fields of a record
let toTuple p = p.Age, p.FirstName, p.FamilyName,p.Email

/// accessing old item in construction of new item
/// note that this creates a new record, it doe snot chnage the old one
let ageUp p = {p with Age = p.Age + 1}
let newEmail newmail p = {p with Email = newmail}

/// match on part of a record
let printFamily = function
    | {FamilyName="Clarke"} as p -> printfn "%s is in Tom's family" p.FirstName
    | {FamilyName = "Smith"} as p -> printfn "%s is in the Smith family" p.FirstName
    | {FirstName=name; FamilyName=nameF} -> printfn "This person is %s %s" name nameF
    
//------------------------------------

[<EntryPoint>] // *
let main argv = // *
    //------------Your tests-----------
    printFamily tomsTwin
    //---------------------------------
    Console.ReadKey() |> ignore // prevents window closing under VS Code
    0 // return an integer exit code