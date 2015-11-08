//
// SchizoMicro F# Referemce Compiler
// Copyright (C) 2014-2015  Wael El Oraiby
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// 
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
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

    | UnaryOp       of string * Exp * Exp
    | BinaryOp      of string * Exp * Exp

    | NativeMacro   of (Environment * Exp list -> Exp) // native syntax transformer
    | Macro         of string list * Exp list // interpreted syntax transformer

    | Environment   of Environment // only native macros can export environments
    | Exception     of Exp


and DebugInfo = {
    Line        : int
    Offset      : int
}

and Environment = {
    SymbolMap   : Map<string, Exp>
    UnaryOps    : Map<string, int>
    BinaryOps   : Map<string, int>
}

type Environment
with
    member x.TryFindSymbol s    = x.SymbolMap.TryFind s
    member x.AddSymbol (s, v)   = { x with SymbolMap = x.SymbolMap.Add (s, v) }

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
    | h :: [] -> h
    | ESymbol s :: t  -> EApplication (ESymbol s, t)
    | EApplication (h, t) :: tl  -> EApplication (EApplication (h, t), tl)
    | _ -> failwith "Expression list is not an application"

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
    | _ -> failwith "ill formed Schizo Expression"

let parse (str: Exp list) : Exp =
    let rec loop (acc: Exp list) (str: Exp list) : Exp list =
        match str with
        | [] -> List.rev acc
        | _ ->
            let tok, nextList = nextToken str
            loop (tok :: acc) nextList
    
    loop [] str
    |> reduceList

type Exp
with
    static member fromString(str: string) = str |> Seq.toList |> List.map EChar |> EList

//    member x.Eval (env: Map<string, Exp>) =

type List<'T>
with
    static member splitAt (pos: int) (l: List<'T>) =
        let _, hl, tl =
            l
            |> List.fold(fun (i: int, hl: List<'T>, tl: List<'T>) e ->
                if i < pos
                then (i + 1, e :: hl, tl)
                else (i + 1, hl, e :: tl)) (0, [], [])
        hl |> List.rev,
        tl |> List.rev

let rec evalCell (env: Environment) c =
    match c with
    | EBoolean  _
    | EChar     _
    | EInt64    _
    | EReal64   _
    | Exception _
    | Environment   _
    | NativeMacro   _
    | Macro     _
    | UnaryOp   _
    | BinaryOp  _
    | EList     _   -> c
    | ESymbol  s    ->
        match env.TryFindSymbol s with
        | Some c -> evalCell env c
        | None   -> Exception ((sprintf "unable to find symbol %s" s) |> Exp.fromString)
    | EApplication (op, opnds) -> apply env op opnds
    | _ -> Exception ("unsupported" |> Exp.fromString)
  
and apply (env: Environment) (operator: Exp) (operands: Exp list) =
    match evalCell env operator with
    | NativeMacro ffi -> ffi (env, operands)

    | Macro (args, el) ->
        let newEnv =
            match args.Length, operands.Length with
            | al, ol when al > ol -> failwith (sprintf "macro requires at least %d arguments, got %d!" args.Length operands.Length)
            | al, ol when al = ol ->
                operands
                |> List.zip args
                |> List.fold (fun (ne: Environment) (k, v) -> ne.AddSymbol (k, v)) env
            | al, ol when al < ol ->    // split operands into two lists, the tail list is then bound to the last argument
                let oh, ot = operands |> List.splitAt (args.Length - 1)
                let remappedOperands = EList ot :: (oh |> List.rev)
                remappedOperands
                |> List.zip args
                |> List.fold (fun ne kv -> ne.AddSymbol kv) env
            | _ -> failwith "unreachable" 

        let _, retVal =
            el
            |> List.fold (fun (env, rv) e ->
                match evalCell env e with
                | Environment ne -> ne, (Exception <| Exp.fromString "Last expression cannot be an environment expansion")
                | rv -> env, rv) (env, EList [])
        retVal

    | _ -> EApplication (operator, operands)    // syntax could not be expanded anymore, return it as is
      

       
[<EntryPoint>]
let main argv =
    [|
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
        "(s 123) () 456 a b 123.hh !hello $bla%bla()aha"
        "(a b (c 10 12 de) (f 10 11 12))"
        "false"
        "true"
        "d true(false)"
        "hello { a b c d; e f g; 1; 2; 3; }"
        "{ a b c d; }"
        "d (1, 2, 3) (1) (ab cd, defg)"
        "e (1, 2, 3) (4, (6, 7)) (ab cd, defg)"
        "[1]"
        "[1; 2; 3; ab cd; (1, 2); (1);]"
        "[1; 2; 3; ab cd; (1, 2); (1); []]"
    |]
    |> Array.iter
        (fun e ->
            printfn "%A" (parse (e |> Seq.toList |> List.map EChar)))

    0 // return an integer exit code