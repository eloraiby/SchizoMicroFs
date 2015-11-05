open System

type SplitterRequirement =
    | MiddleTail
    | OptionalTail
    | MiddleOnly

type Exp =
    | EBoolean      of bool     // true | false
    | EChar         of char
    | EInt64        of int64    
    | EReal64       of double
    | ESymbol       of string
    | EList         of Exp list // [ ... ; ... ]
    | ESequence     of Exp list // { ... ; ... ; }
    | ETuple        of Exp list // ( ... , ... )
    | EApplication  of Exp * Exp list   // (sym | app) exp exp ...

    | NativeLambda  of NativeCall
    | NativeMacro   of NativeCall
    | Lambda        of NormalCall
    | Macro         of NormalCall
    | Environment   of Map<string, Exp>
    | Continuation  of Map<string, Exp> * Exp
    | Exception     of Exp


and NativeCall = {
    ArgCount    : int
    FFI         : Map<string, Exp> * Exp[] -> Exp
}

and NormalCall = {
    Args        : string[]
    Body        : Exp[]
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

let readChar (str: Exp list) : Exp * Exp list =
    match str with
    | EChar '\\' :: EChar ch :: EChar '\'' :: t ->
        match ch with
        | 'n'  -> EChar '\n', t
        | 'r'  -> EChar '\r', t
        | 'a'  -> EChar '\a', t
        | '\'' -> EChar '\'', t
        | _    -> failwith (sprintf "unknown escape character %c after \\ in char" ch)
    | EChar ch   :: EChar '\'' :: t       -> EChar ch, t
    | _               -> failwith "not a well formed char"

let rec readString (acc: Exp list) (str: Exp list) : Exp * Exp list =
    match str with
    | EChar '"'  :: t -> EList (acc |> List.rev), t // "
    | EChar '\\' :: EChar ch :: t ->
        let newCh =
            match ch with
            | 'n'  -> '\n'
            | 'r'  -> '\r'
            | 'a'  -> '\a'
            | '\'' -> '\''
            | _    -> failwith (sprintf "unknown escape character %c after \\ in string" ch)
        readString (EChar newCh :: acc) t
    | ch :: t -> readString (ch :: acc) t
    | _ -> failwith "ill formated string"

let rec readSymbol (acc: Exp list) (str: Exp list) : Exp * Exp list =
    match str with
    | EChar ch :: t when isAlpha ch || isDigit ch || isSpecial ch -> readSymbol (EChar ch :: acc) t
    | _                     ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | EChar ch -> ch | _ -> failwith "unreachable"))
        let sym =
            match sym with
            | "true" -> EBoolean true
            | "false" -> EBoolean false
            | _ -> ESymbol sym
        sym, str    // anything else, just bail

let readNumber (str: Exp list) : Exp * Exp list=
    let rec readInt (acc: Exp list) (str: Exp list) =
        match str with
        | EChar ch :: t when isDigit ch ->  readInt (EChar ch :: acc) t
        | _ -> List.rev acc, str

    let integral, nextList =
        match str with
        | EChar '+' :: t -> let i, nextList = readInt [] t in EChar '+' :: i, nextList
        | EChar '-' :: t -> let i, nextList = readInt [] t in EChar '-' :: i, nextList
        | _ -> readInt [] str

//    match nextList with
//    | '.' :: t ->
//        let decimal, nextList = readInt [] t
//        match nextList with
//        | e :: '+' :: t
//        | e :: t when e = 'E' || e = 'e' ->
//            let expn, nextList = readInt [] t

    let intstr = System.String (integral |> List.toArray |> Array.map (function | EChar ch -> ch | _ -> failwith "unreachable"))

    if intstr.Length <> 0
    then EInt64 (Convert.ToInt64 intstr), nextList
    else failwith "invalid number"
   
let rec skipWS (str: Exp list) : Exp list =
    let isWS = function
        | ' '
        | '\n'
        | '\r' -> true
        | _    -> false

    match str with
    | EChar ch :: t when isWS ch -> skipWS t
    | _ -> str

let reduceList (el: Exp list) : Exp =
    match el with
    |  h :: [] -> h
    |  h :: t  -> EApplication (h, t)
    | _ -> failwith "unreachable"

let rec reduceTuple (e: Exp) =
    match e with
    | ETuple (h :: []) -> reduceTuple h
    | _ -> e


let rec readListOfExp splitterChar endingChar (splitterReq: SplitterRequirement) (boxFunc: Exp list -> Exp) (acc: Exp list) (str: Exp list) =
    let str = skipWS str
    match str with
    | []                                 -> failwith (sprintf "simplified list doesn't have an end '%c'" endingChar)
    | EChar ch :: t when ch = endingChar -> (boxFunc (acc |> List.rev)), t
    | _ ->
        let rec readList (acc: Exp list) (str: Exp list) : Exp * Exp list =
            let str = skipWS str
            match str, splitterReq with
            | [], _                                                     -> failwith (sprintf "list doesn't have an end '%c'" endingChar)
            | EChar ch :: t, MiddleTail when ch = endingChar            -> failwith (sprintf "should have '%c' before having '%c'" splitterChar endingChar)
            | EChar ch :: t, _ when ch = endingChar                     -> (acc |> List.rev |> reduceList), str
            | EChar ch :: t, MiddleOnly when ch = splitterChar          ->
                let str2 = skipWS t
                match str2 with
                | EChar ch :: t when ch = endingChar                    -> failwith (sprintf "shouldn't have '%c' before '%c'" splitterChar endingChar)
                | _                                                     -> (acc |> List.rev |> reduceList), t

            | EChar ch :: t, _ when ch = splitterChar                   -> (acc |> List.rev |> reduceList), t
            | _                                                         -> let tok, nextList = nextToken str in readList (tok :: acc) nextList

        let tok, nextList = readList [] str
        readListOfExp splitterChar endingChar splitterReq boxFunc (tok :: acc) nextList


and readSequence    = readListOfExp ';' '}' MiddleTail ESequence

and readTuple       = readListOfExp ',' ')' MiddleOnly (ETuple >> reduceTuple)

and readList        = readListOfExp ';' ']' OptionalTail EList

and nextToken (str: Exp list) : Exp * Exp list =
    let str = skipWS str
    match str with
    | EChar '\'' :: t -> readChar          t
    | EChar '"'  :: t -> readString     [] t  // "
    | EChar '('  :: t -> readTuple      [] t
    | EChar '{'  :: t -> readSequence   [] t
    | EChar '['  :: t -> readList       [] t
    | EChar sign :: EChar ch :: t when (sign = '+' || sign = '-') && isDigit ch -> readNumber str
    | EChar ch   :: t when isDigit ch -> readNumber str
    | EChar ch   :: t when isAlpha ch  || isSpecial ch -> readSymbol [] str
    | _ -> failwith "ill formed S-Expression"

let parse (str: Exp list) : Exp list =
    let rec loop (acc: Exp list) (str: Exp list) : Exp list =
        match str with
        | [] -> List.rev acc
        | _ ->
            let tok, nextList = nextToken str
            loop (tok :: acc) nextList
    loop [] str

type Exp
with
    static member fromString(str: string) = str |> Seq.toList |> List.map EChar |> EList

//    member x.Eval (env: Map<string, Exp>) =

let rec evalCell (env: Map<string, Exp>) c =
    match c with
    | EBoolean  _
    | EChar     _
    | EInt64    _
    | EReal64   _
    | Exception _
    | Environment   _
    | NativeLambda  _
    | NativeMacro   _
    | Lambda    _
    | EList     _
    | Macro     _   -> c
    | ESymbol  s    ->
        match env.TryFind s with
        | Some c -> evalCell env c
        | None   -> Exception ((sprintf "unable to find symbol %s" s) |> Exp.fromString)
    | EApplication (op, opnds) -> apply env op opnds
    | Continuation  (env, c)    -> evalCell env c
    | _ -> Exception ("unsupported" |> Exp.fromString)

and evalList env (l: Exp list) =
    let arr = Array.init l.Length (fun _ -> EList [])
    let rec loop i =
        if i = l.Length
        then ()
        else
            arr.[i] <- evalCell env l.[i]
            match arr.[i] with
            | Exception _ -> ()
            | _           -> loop (i + 1)
    loop 0
    arr

   
and apply (env: Map<string, Exp>) (operator: Exp) (operands: Exp list) =
    match evalCell env operator with
    | NativeLambda { ArgCount = argc; FFI = ffi } ->
        let args = operands
        if argc <> args.Length
        then Exception ((sprintf "native lambda requires %d arguments" argc) |> Exp.fromString)
        else ffi (env, evalList env args)

    | NativeMacro { ArgCount = argc; FFI = ffi } ->
        let args = operands |> Array.ofList
        if argc <> args.Length
        then Exception ((sprintf "native macro requires %d arguments" argc) |> Exp.fromString)
        else ffi (env, args)       

       
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
        "{a b c;}"
        "(abc def gfhi)"
        "(abc123 d045ef gf.hi)"
        "123 () 456 a b 123.hh !hello $bla%bla()aha"
        "(a b (10 12 cd) (10 11 12))"
        "false"
        "true"
        "true(false)"
        "hello { a b c d; e f g; 1 2 3; }"
        "{ a b c d; }"
        "(1, 2, 3) (1) (ab cd, defg)"
        "(1, 2, 3) (4, (6, 7)) (ab cd, defg)"
        "[1]"
        "[1; 2; 3; ab cd; (1, 2); (1);]"
        "[1; 2; 3; ab cd; (1, 2); (1); []]"
    |]
    |> Array.iter
        (fun e ->
            printfn "%A" (parse (e |> Seq.toList |> List.map EChar)))

    0 // return an integer exit code