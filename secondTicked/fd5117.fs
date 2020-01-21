let lexNGram (ngram: (char list * bool) list) (cLst: char list) : (char list * char list) option=
    // match a non-looping rule
    let takeIfInChars chars (matchedCharList, leftCharList) : (char list * char list) option =
        match leftCharList with
        | [] -> None
        | currentChar :: leftCharList' ->
            if List.exists((=) currentChar) chars 
            then Some (currentChar :: matchedCharList, leftCharList') 
            else None
    
    // match a looping rule
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
            // try to match once ...
            match takeIfInChars charsLst state' with 
            | None -> None // if does not match, return None
            | _ ->  // else, look if the rule can loop and act accordingly
                if canRepeat
                then  Some (takeWhileInChars charsLst state')
                else takeIfInChars charsLst state' 

    // output result
    let result = (Some ([], cLst), ngram) ||> List.fold tryMatch
    match result with
    | None -> None
    | Some (matchedCharList, leftCharList) -> Some (List.rev matchedCharList, leftCharList)    