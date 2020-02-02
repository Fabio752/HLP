open System
open Expecto

///////////////////////////
/////// TOKENISER /////////
///////////////////////////
// Types of accepted tokens.
type Token = 
    | Dot
    | OpenRoundBracket
    | ClosedRoundBracket
    | OpenSquareBracket
    | ClosedSquareBracket
    | Other of string

// If other, build recursively a single token.
let rec buildStringToken token input =
    match input with 
    | currChar::tl when not <| currChar.Equals('.') || 
                               currChar.Equals('(') || 
                               currChar.Equals(')') || 
                               currChar.Equals('[') || 
                               currChar.Equals(']') -> buildStringToken (token @ [currChar]) tl
    | _ -> token, input

let tokeniseT3 (str: string) =
    // Recursively trying to match a token. 
    let rec tokenise input =
        match input with
        | '.'::tl -> [Dot] @ tokenise tl
        | '('::tl -> [OpenRoundBracket] @ tokenise tl
        | ')'::tl -> [ClosedRoundBracket] @ tokenise tl
        | '['::tl -> [OpenSquareBracket] @ tokenise tl
        | ']'::tl -> [ClosedSquareBracket] @ tokenise tl
        | [] -> []
        | _ -> 
            // Other case, lets match the whole string and return it.
            let token, input' = buildStringToken [] input
            let matched = [
                          token 
                          |> List.toArray
                          |> String
                          |> Other 
                          ]
            matched @ tokenise input'
                           
    tokenise <| Seq.toList str 

///////////////////////////
//////// PARSING //////////
///////////////////////////

type AstT3 =
    | ROUNDBRA of AstT3
    | SQBRA of AstT3
    | EXP of AstT3 * AstT3
    | DOTEXP

let parseT3 lst =
    //// ACTIVE PATTERNS ////
    
    // simple one token matching
    let (|ParseToken|_|) oneToken tokenLst =
        match tokenLst with
        | Ok (currToken::restLst) when currToken = oneToken -> Some (Ok restLst)
        | _ -> None

    // recursive round bracket matching -> ROUNDBRA ::= "(" EXP ")"
    let rec (|ParseRound|_|) tokenList =
        match tokenList with
        | ParseToken OpenRoundBracket (ParseExp (Some parsedExp, (ParseToken ClosedRoundBracket rest))) -> Some (Some parsedExp, rest)
        | _ -> None

    // recursive square bracket matching -> SQBRA ::= "[" EXP "]"
    and (|ParseSquare|_|) tokenList =
        match tokenList with
        | ParseToken OpenSquareBracket (ParseExp (Some parsedExp, (ParseToken ClosedSquareBracket rest))) -> Some (Some parsedExp, rest)
        | _ -> None
        
    // parse an exp, based on the top rule of the grammar : EXP ::= "." | ROUNDBRA | SQBRA | ROUNDBRA SQBRA 
    and (|ParseExp|_|) tokenList =
        // try to match part of the token, when hit a good rule return the parsed values and a rest.
        match tokenList with
        | Ok (Dot::rest) -> Some (Some DOTEXP, Ok rest) // it can only match one dot, repeating dots would break the grammar
        | ParseRound (Some ast, Ok rest) -> Some (Some (ROUNDBRA ast), Ok rest)
        | ParseSquare (Some ast, Ok rest) -> Some (Some (SQBRA ast), Ok rest) 
        | ParseRound (Some outerAst, (ParseSquare (Some innerAst, Ok rest))) -> Some (Some (EXP (outerAst, innerAst)), Ok rest) 
        | _ -> failwith "error in expression parsing" 
    
    //// Actual parsing function body ////
    // three possible cases :
    // 1. Matched the whole string -> return ast 
    // 2. Matched part of the string -> return "error indicating the token number (starting from zero) which did not parse and an error message"
    // 3. Not Matched any token -> return error indicating failure at  
    match Ok lst with
        | ParseExp (ast, Ok []) -> Ok ast
        | ParseExp (_, Ok rest) -> 
                    Error <| (List.length lst - List.length rest, sprintf "remaining unmatched: %A" rest)
        | _ -> Error (0, sprintf "Cannot parse at all %A" lst)


///////////////////////////
//////// TESTING //////////
///////////////////////////

// tokenise test
[<Tests>]
let toktest1 =
    testCase "test 1 for tokenise function" <| fun () ->
        Expect.equal (tokeniseT3 "([(...)])") [OpenRoundBracket; OpenSquareBracket; OpenRoundBracket; Dot; Dot; Dot; ClosedRoundBracket; ClosedSquareBracket; ClosedRoundBracket] 
                      "testing tokenising for ([(...)])"

[<Tests>]
let toktest2 =
    testCase "test 2 for tokenise function" <| fun () ->
        Expect.equal (tokeniseT3 "u.u") [Other "u"; Dot; Other "u"] "testing tokenising for u.u"

[<Tests>]
let parsetest1 =
    testCase "parser test 1" <| fun () ->
          Expect.equal (parseT3 [Dot]) (Ok (Some DOTEXP))  "testing parsing for: DOTEXP"

[<Tests>]
let parsetest2 =
    testCase "parser test 2" <| fun () ->
          Expect.equal (parseT3 [OpenRoundBracket; Dot; ClosedRoundBracket]) (Ok (Some (ROUNDBRA DOTEXP)))
            "testing parsing for: ROUNDBRA DOT"

[<Tests>]
let parsetest3 =
    testCase "parser test 3" <| fun () ->
          Expect.equal (parseT3 [OpenSquareBracket; Dot; ClosedSquareBracket]) (Ok (Some (SQBRA DOTEXP))) 
                       "testing parsing for: SQBRA DOT"

[<Tests>]
let parsetest4 =
    testCase "parser test 4" <| fun () ->
          Expect.equal (parseT3 [OpenRoundBracket; OpenSquareBracket; Dot; ClosedSquareBracket; ClosedRoundBracket]) (Ok (Some (ROUNDBRA (SQBRA DOTEXP)))) 
                       "testing parsing for: ROUNDBRA SQBRA DOT"


let runAllTests =
    let testLst =
        testList "Tests" [
            toktest1
            toktest2
            parsetest1
            parsetest2
            parsetest3
            parsetest4
        ]
    runTests defaultConfig testLst |> ignore


[<EntryPoint>]
let main argv =
    runAllTests
    Console.ReadKey() |> ignore
    0 // return an integer exit code