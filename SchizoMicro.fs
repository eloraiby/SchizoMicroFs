open System

type CellData =
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

and Cell = {
    Data        : CellData
    Tags        : Cell list
    DebugInfo   : DebugInfo option
}

and DebugInfo = {
    Line        : int
    Offset      : int
}

type CellData
with
    member x.ToCell = { Cell.Data = x; Tags = []; DebugInfo = None }
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
        | 'n'  -> (CChar '\n').ToCell, t
        | 'r'  -> (CChar '\r').ToCell, t
        | 'a'  -> (CChar '\a').ToCell, t
        | '\'' -> (CChar '\'').ToCell, t
        | _    -> failwith (sprintf "unknown escape character %c after \\ in char" ch)
    | ch   :: '\'' :: t       -> (CChar ch).ToCell, t
    | _               -> failwith "not a well formed char"

let rec readString (acc: char list) (str: char list) =
    match str with
    | '"'  :: t -> (CString (System.String(acc |> List.rev |> Array.ofList))).ToCell, t
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
    | _                     -> (CSymbol (System.String(acc |> List.rev |> Array.ofList))).ToCell, str    // anything else, just bail

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
    then (CInt64 (Convert.ToInt64 intstr)).ToCell, nextList
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
        | [] -> CUnit.ToCell, t
        | _  -> (CList (acc |> List.rev)).ToCell, t
    | _         -> let tok, nextList = nextToken str in readList (tok :: acc) nextList

and nextToken (str: char list) : Cell * char list =
    let isCut =
        function
        | ')' | ' ' | '\r' | '\n' | '\t' -> true
        | _ -> false

    let str = skipWS str
    match str with
    | '\'' :: t -> readChar t
    | '"'  :: t -> readString [] t
    | '('  :: t -> readList [] t
    | 't' :: 'r' :: 'u' :: 'e' :: t -> (CBoolean true).ToCell, t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> (CBoolean false).ToCell, t
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
    |]
    |> Array.iter
        (fun e ->
            printfn "%A" (parse (e |> Seq.toList)))

    0 // return an integer exit code