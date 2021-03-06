﻿//
// SchizoMicro F# Reference Compiler
// Copyright (C) 2014-2016  Wael El Oraiby
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

module Schizo.Tokens

open System

type Token =
    | TokBoolean      of DebugInfo * bool     // true | false
    | TokChar         of DebugInfo * char
    | TokInt64        of DebugInfo * int64    
    | TokReal64       of DebugInfo * double
    | TokIdentifier   of DebugInfo * string
    | TokType         of DebugInfo * string
    | TokGeneric      of DebugInfo * string
    | TokOperator     of DebugInfo * string
    | TokList         of DebugInfo * Token list // [ ... ; ... ]
    | TokScope        of DebugInfo * Token list // { ... ; ... ; }
    | TokTuple        of DebugInfo * Token list // ( ... , ... )
    | TokExpression   of DebugInfo * Token list // (sym | app) exp exp ... | (sym | app | ident) op exp ...
with
    member x.TokenTypeName =
        match x with
        | TokBoolean      (di, y) -> "boolean"
        | TokChar         (di, y) -> "char"
        | TokInt64        (di, y) -> "int"
        | TokReal64       (di, y) -> "real"
        | TokIdentifier   (di, y) -> "identifier"
        | TokType         (di, y) -> "type"
        | TokGeneric      (di, y) -> "generic"
        | TokOperator     (di, y) -> "operator"
        | TokList         (di, y) -> "list"
        | TokScope        (di, y) -> "scope"
        | TokTuple        (di, y) -> "tuple"
        | TokExpression   (di, y) -> "expression"

    override x.ToString() =
        let printList chLeft (y: Token list) chRight sep =
            let str =
                y
                |> List.fold (fun state tok ->
                    match state with
                    | "" -> sprintf "%O" tok
                    | _  -> sprintf "%s%c %O" state sep tok) ""
            sprintf "%c%s%c" chLeft str chRight

        match x with
        | TokBoolean      (_, y) -> y.ToString()
        | TokChar         (_, y) -> sprintf "'%c'" y
        | TokInt64        (_, y) -> y.ToString()
        | TokReal64       (_, y) -> y.ToString()
        | TokIdentifier   (_, y) -> y
        | TokType         (_, y) -> sprintf "*%s*" y
        | TokGeneric      (_, y) -> sprintf "`%s`" y
        | TokOperator     (_, y) -> y
        | TokList         (_, y) -> printList '[' y ']' ';'
        | TokScope        (_, y) -> printList '{' y '}' ';'
        | TokTuple        (_, y) -> printList '(' y ')' ','
        | TokExpression   (_, y) -> printList '~' y '~' ' '

and DebugInfo = {
    Source          : string
    Line            : int
    Offset          : int
} with
    static member empty = { Source = ""; Line = 0; Offset = 0 }



let private isAlpha ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch = '_')

let private isDigit ch = ch >= '0' && ch <= '9'

let private isUpper ch = ch >= 'A' && ch <= 'Z'

let private isSpecial =
    function
    | '`'   | '!'   | '@'    
    | '$'   | '%'   | '^'   | '&' 
    | '*'   | '-'   | '+'   | '='
    | '/'   | '?'   | '<'   | '>'
    | '|'   | '~'   | '.'   | ':' -> true
    | _   -> false

let private readChar (str: Token list) : Token * Token list =
    match str with
    | TokChar (di, '\\') :: TokChar (_, ch) :: TokChar (_, '\'') :: t ->
        match ch with
        | 'n'  -> TokChar (di, '\n'), t
        | 'r'  -> TokChar (di, '\r'), t
        | 'a'  -> TokChar (di, '\a'), t
        | '\'' -> TokChar (di, '\''), t
        | _    -> failwith (sprintf "unknown escape character %c after \\ in char" ch)
    | TokChar (di, ch) :: TokChar (_, '\'') :: t       -> TokChar (di, ch), t
    | _               -> failwith "readChar: not a well formed char"

let rec private readString (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar (di, '"')  :: t -> TokList (di, (acc |> List.rev)), t // "
    | TokChar (di, '\\') :: TokChar (_, ch) :: t ->
        let newCh =
            match ch with
            | 'n'  -> '\n'
            | 'r'  -> '\r'
            | 'a'  -> '\a'
            | '\'' -> '\''
            | _    -> failwith (sprintf "unknown escape character %c after \\ in string" ch)
        readString (TokChar (di, newCh) :: acc) t
    | ch :: t -> readString (ch :: acc) t
    | _ -> failwith "readString: ill formated string"

let rec private readSymbolAlphaNum di (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar (_di, ch) :: t when isAlpha ch || isDigit ch || (acc = [] && ch = '#') -> readSymbolAlphaNum di (TokChar (_di, ch) :: acc) t
    | _ ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "unreachable"))
        let sym =
            match sym with
            | ""      -> failwith "invalid identifier (null)"
            | "true"  -> TokBoolean (di, true)
            | "false" -> TokBoolean (di, false)
            | str when str.[0] |> isUpper -> TokType    (di, sym)
            | str when str.[0] = '#'      -> TokGeneric (di, sym)
            | str     -> TokIdentifier (di, sym)
        sym, str    // anything else, just bail

let rec private readSymbolSpecial di (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar (_di, ch) :: t when isSpecial ch -> readSymbolSpecial di (TokChar (_di, ch) :: acc) t
    | TokChar (_di, ch) :: _                     ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "readSymbolSpecial: unreachable"))
        TokOperator (di, sym), str    // anything else, just bail
    | _ -> failwith "readSymbolSpecial: unreachable"
//
// floating point grammar
// D   [0-9]
// E   ([Ee][+-]?{D}+)
//
// {D}+"."{D}+{E}?
//
let private readNumber di (str: Token list) : Token * Token list=
    let rec readInt (acc: Token list) (str: Token list) : Token list * Token list =
        match str with
        | TokChar (di, ch) :: t when isDigit ch ->  readInt (TokChar (di, ch) :: acc) t
        | _ -> List.rev acc, str

    let integral, nextList =
        match str with
        | TokChar (di, '+') :: t -> let i, nextList = readInt [] t in TokChar (di, '+') :: i, nextList
        | TokChar (di, '-') :: t -> let i, nextList = readInt [] t in TokChar (di, '-') :: i, nextList
        | _ -> readInt [] str

    let decimal, nextList =
        match nextList with
        | TokChar (di, '.') :: t -> let d, nextList = readInt [] t in TokChar (di, '.') :: d, nextList
        | _ -> [], nextList

    let expo, nextList =
        match decimal, nextList with
        | [], _ -> [], nextList
        | _, TokChar (di, 'E') :: t
        | _, TokChar (di, 'e') :: t ->
            match t with
            | TokChar (di2, '+') :: t -> let i, nextList = readInt [] t in (TokChar (di, 'E')) :: (TokChar (di2, '+')) :: i, nextList
            | TokChar (di2, '-') :: t -> let i, nextList = readInt [] t in (TokChar (di, 'E')) :: (TokChar (di2, '-')) :: i, nextList
            | t -> readInt [] t
        | _, _ -> [], nextList
        

    let token, nextList =
        match integral, decimal, expo with
        | integral, [], _ ->
            let intstr = System.String (integral |> List.toArray |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "unreachable"))

            if intstr.Length <> 0
            then TokInt64 (di, Convert.ToInt64 intstr), nextList
            else failwith "invalid integer number"

        | integral, decimal, [] ->
            let number =  List.append integral decimal 
            let number = System.String (number |> List.toArray |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "unreachable"))

            if number.Length <> 0
            then TokReal64 (di, Convert.ToDouble number), nextList
            else failwith "invalid floating number"

        | integral, decimal, expo ->
            let number = List.append  (List.append integral decimal) expo
            let number = System.String (number |> List.toArray |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "unreachable"))

            if number.Length <> 0
            then TokReal64 (di, Convert.ToDouble number), nextList
            else failwith "invalid floating number"

    match nextList with
    | TokChar (_, ch) :: l when isAlpha ch -> failwith "invalid number: followed by a alpha or special character, separator or space is needed"
    | _ -> token, nextList
   
let rec private skipWS (str: Token list) : Token list =
    let isWS = function
        | ' '
        | '\n'
        | '\r' -> true
        | _    -> false

    match str with
    | TokChar (_, ch) :: t when isWS ch -> skipWS t
    | _ -> str


let rec private reduceTuple (e: Token) =
    match e with
    | TokTuple (di, (h :: [])) -> reduceTuple h
    | _ -> e

let reduceExpression di (toks: Token list) =
    match toks with
    | h :: [] -> h
    | _       -> TokExpression (di, toks)

let curr2Tup f x y = f(x, y)

let rec private readListOfTokens di splitterChar endingChar (boxFunc: DebugInfo -> Token list -> Token) (acc: Token list) (str: Token list) : Token * Token list =
    let str = skipWS str
    match str with
    | []                                 -> failwith (sprintf "%s doesn't have an end '%c'" ((boxFunc di []).TokenTypeName) endingChar)
    | TokChar (_, ch) :: t when ch = endingChar -> let exp = (boxFunc di (acc |> List.rev)) in exp, t
    | _ ->
        let rec readList (acc: Token list) (str: Token list) : Token * Token list =
            let str = skipWS str
            match str with
            | []                                                        -> failwith (sprintf "%s doesn't have an end '%c'" ((boxFunc di []).TokenTypeName) endingChar)
            | TokChar (_, ch) :: t when ch = endingChar                 -> let exp = acc |> List.rev |> reduceExpression di in exp, str
            | TokChar (_, ch) :: t when ch = splitterChar               ->
                match skipWS t with
                | TokChar (_, ch) :: t when ch = endingChar             -> failwith (sprintf "%s ends with splitter '%c'" ((boxFunc di []).TokenTypeName) splitterChar)
                | _     -> let exp = acc |> List.rev |> reduceExpression di in exp, t
            | _                                                         -> let tok, nextList = nextToken str in readList (tok :: acc) nextList

        let tok, nextList = readList [] str
        readListOfTokens di splitterChar endingChar boxFunc (tok :: acc) nextList

and private readScope di = readListOfTokens di ';' '}' (curr2Tup TokScope)

and private readTuple di = readListOfTokens di ',' ')' (curr2Tup (TokTuple >> reduceTuple))

and private readList  di = readListOfTokens di ';' ']' (curr2Tup TokList)

and private nextToken (str: Token list) : Token * Token list =
    let str = skipWS str
    match str with
    | TokChar (di, '\'') :: t -> readChar        t
    | TokChar (di, '"' ) :: t -> readString   [] t // "
    | TokChar (di, '(' ) :: t -> readTuple    di [] t
    | TokChar (di, '{' ) :: t -> readScope    di [] t
    | TokChar (di, '[' ) :: t -> readList     di [] t
    | TokChar (di, sign) :: TokChar (_, ch) :: t when (sign = '+' || sign = '-') && isDigit ch -> readNumber di str
    | TokChar (di, ch  ) :: t when isDigit ch   -> readNumber di str
    | TokChar (di, ch  ) :: t when isAlpha ch || ch = '#' -> readSymbolAlphaNum di [] str
    | TokChar (di, ch  ) :: t when isSpecial ch -> readSymbolSpecial di [] str
    | _ -> failwith "ill formed Schizo Token"

type Token
with
    static member tokenize (str: Token list) : Token =
        let rec loop (acc: Token list) (str: Token list) : Token list =
            match str with
            | [] -> List.rev acc
            | _ ->
                let tok, nextList = nextToken str
                loop (tok :: acc) nextList
        
        let expList = loop [] str
        expList |> curr2Tup TokExpression DebugInfo.empty