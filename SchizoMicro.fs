open System

type Cell =
    | CUnit
    | CBoolean  of bool
    | CChar     of char
    | CInt64    of int64
    | CReal64   of double
    | CString   of string
    | CSymbol   of string
    | CList     of Cell list
    | NativeLambda  of NativeCall
    | NativeMacro   of NativeCall
    | Lambda    of NormalCall
    | Macro     of NormalCall
    | Environment of Map<string, Cell>

and NativeCall = {
    ArgCount    : int
    FFI         : Cell[] -> Cell
}

and NormalCall = {
    Args        : string[]
    Body        : Cell list
}

and DebugInfo = {
    Line        : int
    Offset      : int
}

let isAlpha ch =
    if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
    then true
    else false

let isDigit ch =
    if ch >= '0' && ch <= '9'
    then true
    else false

let isSpecial =
    function
    | '`'   | '!'   | '@'   | '#'  
    | '$'   | '%'   | '^'   | '&'  
    | '*'   | '_'   | '-'   | '+'  
    | '='   | '/'   | '?'   | '<'  
    | '>'   | '|'   | '~'   | '.' -> true
    | _   -> false

let readChar (str: char list) =
    match str with
    | '\\' :: ch :: '\'' :: t ->
        match ch with
        | 'n'  -> CChar '\n', t
        | 'r'  -> CChar '\r', t
        | 'a'  -> CChar '\a', t
        | '\'' -> CChar '\'', t
        | _    -> failwith (sprintf "unknown escape character %c after \\ in char" ch)
    | ch   :: '\'' :: t       -> CChar ch, t
    | _               -> failwith "not a well formed char"

let rec readString (acc: char list) (str: char list) =
    match str with
    | '"'  :: t -> CString (System.String(acc |> List.rev |> Array.ofList)), t
    | '\\' :: ch :: t ->
        let newCh =
            match ch with
            | 'n'  -> '\n'
            | 'r'  -> '\r'
            | 'a'  -> '\a'
            | '\'' -> '\''
            | _    -> failwith (sprintf "unknown escape character %c after \\ in string" ch)
        readString (newCh :: acc) t
    | ch :: t -> readString (ch :: acc) t
    | _ -> failwith "ill formated string"

let rec readSymbol (acc: char list) (str: char list) =
    match str with
    | ch :: t when isAlpha ch || isDigit ch || isSpecial ch -> readSymbol (ch :: acc) t
    | _                     ->
        let sym = System.String(acc |> List.rev |> Array.ofList)
        let sym =
            match sym with
            | "true" -> CBoolean true
            | "false" -> CBoolean false
            | _ -> CSymbol sym
        sym, str    // anything else, just bail

let readNumber (str: char list) =
    let rec readInt (acc: char list) (str: char list) =
        match str with
        | ch :: t when isDigit ch ->  readInt (ch :: acc) t
        | _ -> List.rev acc, str

    let integral, nextList =
        match str with
        | '+' :: t -> let i, nextList = readInt [] t in '+' :: i, nextList
        | '-' :: t -> let i, nextList = readInt [] t in '-' :: i, nextList
        | _ -> readInt [] str

//    match nextList with
//    | '.' :: t ->
//        let decimal, nextList = readInt [] t
//        match nextList with
//        | e :: '+' :: t
//        | e :: t when e = 'E' || e = 'e' ->
//            let expn, nextList = readInt [] t

    let intstr = System.String (integral |> List.toArray)

    if intstr.Length <> 0
    then CInt64 (Convert.ToInt64 intstr), nextList
    else failwith "invalid number"
    
let rec skipWS (str: char list) =
    let isWS = function
        | ' ' 
        | '\n'
        | '\r' -> true
        | _    -> false

    match str with
    | ch :: t when isWS ch -> skipWS t
    | _ -> str

let rec readList (acc: Cell list) (str: char list) =
    let str = skipWS str
    match str with
    | []        -> failwith "list doesn't have an end ')'"
    | ')' :: t  ->
        match acc with
        | [] -> CUnit, t
        | _  -> CList (acc |> List.rev), t
    | _         -> let tok, nextList = nextToken str in readList (tok :: acc) nextList

and nextToken (str: char list) : Cell * char list =
    let str = skipWS str
    match str with
    | '\'' :: t -> readChar t
    | '"'  :: t -> readString [] t
    | '('  :: t -> readList [] t
    | sign :: ch :: t when (sign = '+' || sign = '-') && isDigit ch -> readNumber str
    | ch   :: t when isDigit ch -> readNumber str 
    | ch   :: t when isAlpha ch  || isSpecial ch -> readSymbol [] str
    | _ -> failwith "ill formed S-Expression" 

let parse (str: char list) : Cell list =
    let rec loop (acc: Cell list) (str: char list) : Cell list =
        match str with
        | [] -> List.rev acc
        | _ ->
            let tok, nextList = nextToken str
            loop (tok :: acc) nextList
    loop [] str

[<EntryPoint>]
let main argv =
    [|
        ""
        "'c'"
        "123"
        "0"
        "symbol"
        "\"string\""
        "( )"
        "'\\n'"
        "a b c d"
        "(abc def gfhi)"
        "(abc123 d045ef gf.hi)"
        "123 () 456 a b 123.hh !hello $bla%bla()aha"
        "(a b (10 12 cd) (10 11 12))"
        "false"
        "true"
        "true(false)"
    |]
    |> Array.iter
        (fun e ->
            printfn "%A" (parse (e |> Seq.toList)))

    0 // return an integer exit code