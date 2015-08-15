open System

type Token =
    | TChar     of char
    | TInt64    of int64
    | TReal64   of double
    | TString   of string
    | TSymbol   of string
    | LParen
    | RParen
    | LBrace
    | RBrace
    | LBrack
    | RBrack
    | Tag
    | Comma
    | SC

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

let isWS = function
    | ' ' 
    | '\n'
    | '\r' -> true
    | _    -> false

let readChar (str: char list) =
    match str with
    | '\\' :: ch :: '\'' :: t ->
        match ch with
        | 'n'  -> TChar '\n', t
        | 'r'  -> TChar '\r', t
        | 'a'  -> TChar '\a', t
        | '\'' -> TChar '\'', t
        | _    -> failwith (sprintf "unknown escape character %c after \\ in char" ch)
    | ch   :: '\'' :: t       -> TChar ch, t
    | _               -> failwith "not a well formed char"

let rec readString (acc: char list) (str: char list) =
    match str with
    | '"'  :: t -> TString (System.String(acc |> List.rev |> Array.ofList)), t
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
    | ch :: t when isWS ch  -> TSymbol (System.String(acc |> List.rev |> Array.ofList)), t      // white space, bail
    | _                     -> TSymbol (System.String(acc |> List.rev |> Array.ofList)), str    // anything else, just bail

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
    then TInt64 (Convert.ToInt64 intstr), nextList
    else failwith "invalid number"
    
let rec lex (tlist: Token list) (str: char list) : Token list =
    match str with
    | '\'' :: t -> let ch,  nextList = readChar t      in lex (ch  :: tlist) nextList
    | '"'  :: t -> let str, nextList = readString [] t in lex (str :: tlist) nextList
    | '('  :: t -> lex (LParen :: tlist) t
    | ')'  :: t -> lex (RParen :: tlist) t
    | '{'  :: t -> lex (LBrace :: tlist) t
    | '}'  :: t -> lex (RBrace :: tlist) t
    | '['  :: t -> lex (LBrack :: tlist) t
    | ']'  :: t -> lex (RBrack :: tlist) t
    | ','  :: t -> lex (Comma  :: tlist) t
    | ';'  :: t -> lex (SC     :: tlist) t
    | '+'  :: ch :: t
    | '-'  :: ch :: t
    | ch   :: t when isDigit ch -> let num, nextList = readNumber str in lex (num :: tlist) nextList
    | ch   :: t when isAlpha ch  || isSpecial ch -> let sym, nextList = readSymbol [] str in lex (sym :: tlist) nextList
    | ch   :: t when isWS ch -> lex tlist t
    | [] -> List.rev tlist
    | _ -> failwith "ill formed S-Expression"

type Cell =
    | CUnit
    | CChar     of char
    | CInt64    of int64
    | CReal64   of double
    | CString   of string
    | CSymbol   of string
    | CList     of Cell list

with
    static member fromToken (t: Token) =
        match t with
        | TChar     t -> CChar   t
        | TInt64    t -> CInt64  t
        | TReal64   t -> CReal64 t
        | TString   t -> CString t
        | TSymbol   t -> CSymbol t
        | _           -> failwith "unrecognized token"

let rec readSExp (acc: Cell list) (tlist: Token list) : Cell * (Token list) =
    match tlist with
    | LParen :: RParen :: t -> readSExp (CUnit :: acc) t
    | LParen :: t -> readSExp [] t
    | RParen :: t -> CList (List.rev acc), t
    | cell   :: t -> readSExp ((Cell.fromToken cell) :: acc) t

[<EntryPoint>]
let main argv =
    [|
        ""
        "'c'"
        "123"
        "0"
        "symbol"
        "\"string\""
        "("
        ")"
        "'\\n'"
        "a b c d"
        "(abc def gfhi)"
        "(abc123 d045ef gf.hi)"
        "123 () 456 a b 123.hh !hello $bla%bla()aha"
    |]
    |> Array.iter
        (fun e ->
            printfn "%A" (lex [] (e |> Seq.toList)))

    0 // return an integer exit code