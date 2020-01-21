open System

//print function
let print x = printfn "%A%s" x Environment.NewLine

let lexNGramF (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option=
    // match a non-looping rule
    let takeIfInChars chars (matchedCharList, leftCharList) : (char list * char list) option =
        match leftCharList with
        | [] -> None
        | currentChar :: leftCharList' ->
            if List.exists((=) currentChar) chars 
            then Some (currentChar :: matchedCharList, leftCharList') 
            else None
    
    // because of looping same rule can be applied to a series of char
    let rec takeWhileInChars chars (matchedCharList, leftCharList) = 
        match leftCharList with 
        | [] -> (matchedCharList, leftCharList) 
        | currentChar::leftCharList' ->  
            if List.exists((=) currentChar) chars 
            then (takeWhileInChars chars (currentChar::matchedCharList, leftCharList')) // recursive call
            else (matchedCharList, leftCharList)

    let tryMatch state (charsLst, canRepeat) =
        match state with
        | None -> None
        | Some state' ->
            // try to match once
            match takeIfInChars charsLst state' with 
            | None -> None // if not, rule failed
            | _ ->  // else, look if the rule can loop and act accordingly
                if canRepeat
                then  Some (takeWhileInChars charsLst state')
                else takeIfInChars charsLst state' 

    let result = (Some ([], cLst), ngram) ||> List.fold tryMatch
    match result with
    | None -> None
    | Some (matchedCharList, leftCharList) -> Some (List.rev matchedCharList, leftCharList)    


let lexNGramO (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option=
    let rec takeWhileInChars chars (acc,lst) =
        match lst with
        | head::tail -> 
            if List.contains head chars 
            then (takeWhileInChars chars (acc @ [head], tail)) 
            else (acc, lst)
        | _ -> (acc, lst)

    let tryMatch state (charsLst,canRepeat) =
        match state with
        | None -> None
        | Some state' ->
            match snd state' with
            | head::tail when List.contains head charsLst -> 
                if canRepeat = true 
                then Some (takeWhileInChars charsLst state') 
                else Some (fst state' @ [head], tail)
            | _ -> None

    (Some ([], cLst), ngram) ||> List.fold tryMatch    


let lexNGramS (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option=
    // matches exactly 1 char
    let takeIfInChars chars (acc,lst) : (char list * char list) option =
        match lst with
        | []     -> None
        | hd::tl -> if List.exists ((=) hd) chars
                    then Some ( hd::acc , tl )
                    else None      
    // matches 0 or more chars
    let rec takeWhileInChars chars (acc,lst) =
        match lst with
        | []     -> (acc,lst)
        | hd::tl -> if List.exists ((=) hd) chars
                    then takeWhileInChars chars (hd::acc,tl)
                    else (acc,lst)
        
    let tryMatch state (charsLst,canRepeat) =
        match state with
        | None -> None
        | Some s ->
            // always matches at least 1 char
            (takeIfInChars charsLst s, canRepeat)
            |> function
            | None,       _ -> None
            | res,    false -> res
            | Some s1, true -> Some (takeWhileInChars charsLst s1)
        
    List.fold tryMatch (Some([],cLst)) ngram
    |> function
    | Some (l,r) -> Some (List.rev l, r)
    | None -> None 


let lexNGramM (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option =
  let takeIfInChars chars (tokL, inL) : (char list * char list) option =
    match inL with
    | [] -> None // We are trying to match a rule but there are no chars left to
                 // match.
    | nextChar :: inL' ->
      match List.tryFind ((=) nextChar) chars with
      | None -> None
      | Some matchedChar -> Some (matchedChar :: tokL, inL')

  let rec takeWhileInChars chars (tokL, inL) step =
    match takeIfInChars chars (tokL, inL) with
    | None ->
      if step = 0
      then None // The rule matched zero times.
      else Some (tokL, inL) // The rule matched for a while then stopped. Ok.
    | Some (tokL', inL') ->
      takeWhileInChars chars (tokL', inL') (step + 1) // Try to match again. 

  let tryMatch state (charsLst, canRepeat) =
    match state with
    | None -> None // We could not match a previous rule. Keep on failing.
    | Some curState -> // curState is a tuple like (tokL, inL).
      if canRepeat
      then takeWhileInChars charsLst curState 0
      else takeIfInChars charsLst curState

  (Some ([], cLst), ngram)
  ||> List.fold tryMatch
  |> function
     | None -> None
     | Some (tokL, inL) -> Some (List.rev tokL, inL) // Need to reverse the list
                                                     // of tokens because they
                                                     // were appended to the 
                                                     // head instead of the 
                                                     // back.
//change this to run
let lexNGram = lexNGramF 


let decimalLit =
    [
        ['0'..'9'],true
        ['.';','],false
        ['0'..'9'],true
    ]
let integerLit =
    [
        ['0'..'9'],true
    ]  
let stringLit =
    [   
        ['\"'],false
        ['0'..'9']@['a'..'z']@['A'..'Z']@[' '],true
        ['\"'],false
    ] 
let emptyStringLit =
    [
        ['\"'],false
        ['\"'],false
    ]
let emailLit =
    [
        ['a'..'z']@['A'..'Z']@['0'..'9'],true
        ['@'],false
        ['a'..'z']@['A'..'Z'],true
        ['.'],false
        ['c'],false
        ['o'],false
        ['m'],false
    ]

let test_decimal = 
    [
        Seq.toList "15.7 rabbits";
        Seq.toList "rabbits 15.7";
        Seq.toList "100000.0123456789";
        Seq.toList " 1.0";
        Seq.toList "4.";
        Seq.toList "6.9S";
    ]
let test_int = 
    [
        Seq.toList "351684";
        Seq.toList "-351684";
        Seq.toList "100000.0123456789";
        Seq.toList " 1";
        Seq.toList "4abc";
        Seq.toList "69S";
    ]
let test_string = 
    [
        Seq.toList "\" \""
        Seq.toList "\"157 rabbits\"";
        Seq.toList "\"RabBIts 157\"";
        Seq.toList "\"1000000123456789\"";
        Seq.toList "\" 10\"";
        Seq.toList "\"4 \"";
        Seq.toList "\"69S\"";
        Seq.toList "A\"69S\"";
        Seq.toList " \"69S\"";
        Seq.toList ".\"69S\"";
        Seq.toList "\"\"";
    ]
let test_empty_string = 
    [
        Seq.toList "\"\""
        Seq.toList " \"\""
        Seq.toList "\"\" "
    ]

let bigNumber = 10000
let testemail =
    [
        Seq.toList "a1@ml.com"
        Seq.toList "a1@ml.com ,.;|]"
        Seq.toList "@m.com"
        Seq.toList "abcdefghijklmnoprstuvwxyz@ABCDEFGHIJKLMNOPRSTUVWXYZ.com"
        Seq.toList "1234567890@GmAiL.commmm"
        (List.map (fun x -> 'a') [1..bigNumber])@(Seq.toList "@m.coma")
    ]


let expected_decimal = 
    [
        Some(['1';'5';'.';'7'],[' ';'r';'a';'b';'b';'i';'t';'s']);
        None;
        Some(['1';'0';'0';'0';'0';'0';'.';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'], []);
        None;
        None;
        Some(['6';'.';'9'], ['S'])
    ]
let expected_int = 
    [
        Some(['3';'5';'1';'6';'8';'4'],[]);
        None;
        Some(['1';'0';'0';'0';'0';'0'],['.';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9']);
        None;
        Some(['4'],['a';'b';'c']);
        Some(['6';'9'], ['S'])
    ]
let expected_string = 
    [
        Some(['"';' ';'"'],[]);
        Some(['"';'1';'5';'7';' ';'r';'a';'b';'b';'i';'t';'s';'"'],[]);
        Some(['"';'R';'a';'b';'B';'I';'t';'s';' ';'1';'5';'7';'"'],[]);
        Some(['"';'1';'0';'0';'0';'0';'0';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'"'], []);
        Some(['"';' ';'1';'0';'"'],[]);
        Some(['"';'4';' ';'"'],[]);
        Some(['"';'6';'9';'S';'"'],[]);
        None;
        None;
        None;
        None;
    ]
let expected_empty_string = 
    [
        Some(['"';'"'],[]);
        None;
        Some(['"';'"'],[' ']);
    ]
let expectedemail =
    [
        Some(['a';'1';'@';'m';'l';'.';'c';'o';'m'],[]);
        Some(['a';'1';'@';'m';'l';'.';'c';'o';'m'],[' ';',';'.';';';'|';']';]);
        None;
        Some(['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p'
             ;'r';'s';'t';'u';'v';'w';'x';'y';'z';'@';'A';'B';'C';'D';'E';'F'
             ;'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'R';'S';'T';'U';'V';'W';
             'X';'Y';'Z';'.';'c';'o';'m';],[]);
        Some(['1';'2';'3';'4';'5';'6';'7';'8';'9';'0';'@';'G';'m';'A';'i';'L';
              '.';'c';'o';'m'],['m';'m';'m';]);
        Some( (List.map (fun x -> 'a') [1..bigNumber])@(Seq.toList "@m.com"), ['a'])
    ]


[<EntryPoint>]
    
let main argv =
    printf "Failures decimal:\n"
    test_decimal
    |> List.map (lexNGram decimalLit)
    |> List.zip expected_decimal
    |> List.filter (fun (expected, actual) -> expected <> actual)
    |> print

    printf "Failures pos int:\n"
    test_int
    |> List.map (lexNGram integerLit)
    |> List.zip expected_int
    |> List.filter (fun (expected, actual) -> expected <> actual)
    |> print
    
    printf "Failures string:\n"
    test_string
    |> List.map (lexNGram stringLit)
    |> List.zip expected_string
    |> List.filter (fun (expected, actual) -> expected <> actual)
    |> print

    printf "Failures empty string:\n"
    test_empty_string
    |> List.map (lexNGram emptyStringLit)
    |> List.zip expected_empty_string
    |> List.filter (fun (expected, actual) -> expected <> actual)
    |> print

    printf "Failures empty email:\n"
    testemail
    |> List.map (lexNGram emailLit)
    |> List.zip expectedemail
    |> List.filter (fun (expected, actual) -> expected <> actual)
    |> print
    
    Console.ReadKey() |> ignore
    0 // return an integer exit code



