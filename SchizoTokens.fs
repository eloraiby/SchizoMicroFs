//
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
    | TokBoolean      of bool     // true | false
    | TokChar         of char
    | TokInt64        of int64    
    | TokReal64       of double
    | TokIdentifier   of string
    | TokOperator     of string
    | TokList         of Token list // [ ... ; ... ]
    | TokScope        of Token list // { ... ; ... ; }
    | TokTuple        of Token list // ( ... , ... )
    | TokExpression   of Token list // (sym | app) exp exp ... | (sym | app | ident) op exp ...
with
    member x.TokenTypeName =
        match x with
        | TokBoolean      y -> "boolean"
        | TokChar         y -> "char"
        | TokInt64        y -> "int"
        | TokReal64       y -> "real"
        | TokIdentifier   y -> "identifier"
        | TokOperator     y -> "operator"
        | TokList         y -> "list"
        | TokScope        y -> "scope"
        | TokTuple        y -> "tuple"
        | TokExpression   y -> "expression"

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
        | TokBoolean      y -> y.ToString()
        | TokChar         y -> sprintf "'%c'" y
        | TokInt64        y -> y.ToString()
        | TokReal64       y -> y.ToString()
        | TokIdentifier   y -> y
        | TokOperator     y -> y
        | TokList         y -> printList '[' y ']' ';'
        | TokScope        y -> printList '{' y '}' ';'
        | TokTuple        y -> printList '(' y ')' ','
        | TokExpression   y -> printList '~' y '~' ' '

let private isAlpha ch =
    if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch = '_')
    then true
    else false

let private isDigit ch =
    if ch >= '0' && ch <= '9'
    then true
    else false

let private isSpecial =
    function
    | '`'   | '!'   | '@'   | '#' 
    | '$'   | '%'   | '^'   | '&' 
    | '*'   | '-'   | '+'   | '='
    | '/'   | '?'   | '<'   | '>'
    | '|'   | '~'   | '.'   | ':' -> true
    | _   -> false

let private readChar (str: Token list) : Token * Token list =
    match str with
    | TokChar '\\' :: TokChar ch :: TokChar '\'' :: t ->
        match ch with
        | 'n'  -> TokChar '\n', t
        | 'r'  -> TokChar '\r', t
        | 'a'  -> TokChar '\a', t
        | '\'' -> TokChar '\'', t
        | _    -> failwith (sprintf "unknown escape character %c after \\ in char" ch)
    | TokChar ch   :: TokChar '\'' :: t       -> TokChar ch, t
    | _               -> failwith "not a well formed char"

let rec private readString (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar '"'  :: t -> TokList (acc |> List.rev), t // "
    | TokChar '\\' :: TokChar ch :: t ->
        let newCh =
            match ch with
            | 'n'  -> '\n'
            | 'r'  -> '\r'
            | 'a'  -> '\a'
            | '\'' -> '\''
            | _    -> failwith (sprintf "unknown escape character %c after \\ in string" ch)
        readString (TokChar newCh :: acc) t
    | ch :: t -> readString (ch :: acc) t
    | _ -> failwith "ill formated string"

let rec private readSymbolAlphaNum (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar ch :: t when isAlpha ch || isDigit ch -> readSymbolAlphaNum (TokChar ch :: acc) t
    | _ ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | TokChar ch -> ch | _ -> failwith "unreachable"))
        let sym =
            match sym with
            | "true"  -> TokBoolean true
            | "false" -> TokBoolean false
            | _ -> TokIdentifier sym
        sym, str    // anything else, just bail

let rec private readSymbolSpecial (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar ch :: t when isSpecial ch -> readSymbolSpecial (TokChar ch :: acc) t
    | _                     ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | TokChar ch -> ch | _ -> failwith "unreachable"))
        TokOperator sym, str    // anything else, just bail
//
// floating point grammar
// D   [0-9]
// E   ([Ee][+-]?{D}+)
//
// {D}+"."{D}+{E}?
//
let private readNumber (str: Token list) : Token * Token list=
    let rec readInt (acc: Token list) (str: Token list) =
        match str with
        | TokChar ch :: t when isDigit ch ->  readInt (TokChar ch :: acc) t
        | _ -> List.rev acc, str

    let integral, nextList =
        match str with
        | TokChar '+' :: t -> let i, nextList = readInt [] t in TokChar '+' :: i, nextList
        | TokChar '-' :: t -> let i, nextList = readInt [] t in TokChar '-' :: i, nextList
        | _ -> readInt [] str

    let decimal, nextList =
        match nextList with
        | TokChar '.' :: t -> let d, nextList = readInt [] t in TokChar '.' :: d, nextList
        | _ -> [], nextList

    let expo, nextList =
        match decimal, nextList with
        | [], _ -> [], nextList
        | _, TokChar 'E' :: t
        | _, TokChar 'e' :: t ->
            match t with
            | TokChar '+' :: t -> let i, nextList = readInt [] t in TokChar 'E' :: TokChar '+' :: i, nextList
            | TokChar '-' :: t -> let i, nextList = readInt [] t in TokChar 'E' :: TokChar '-' :: i, nextList
            | t -> readInt [] t
        | _, _ -> [], nextList
        

    let token, nextList =
        match integral, decimal, expo with
        | integral, [], _ ->
            let intstr = System.String (integral |> List.toArray |> Array.map (function | TokChar ch -> ch | _ -> failwith "unreachable"))

            if intstr.Length <> 0
            then TokInt64 (Convert.ToInt64 intstr), nextList
            else failwith "invalid integer number"

        | integral, decimal, [] ->
            let number =  List.append integral decimal 
            let number = System.String (number |> List.toArray |> Array.map (function | TokChar ch -> ch | _ -> failwith "unreachable"))

            if number.Length <> 0
            then TokReal64 (Convert.ToDouble number), nextList
            else failwith "invalid floating number"

        | integral, decimal, expo ->
            let number = List.append  (List.append integral decimal) expo
            let number = System.String (number |> List.toArray |> Array.map (function | TokChar ch -> ch | _ -> failwith "unreachable"))

            if number.Length <> 0
            then TokReal64 (Convert.ToDouble number), nextList
            else failwith "invalid floating number"

    match nextList with
    | TokChar ch :: l when isAlpha ch -> failwith "invalid number: followed by a alpha or special character, separator or space is needed"
    | _ -> token, nextList
   
let rec private skipWS (str: Token list) : Token list =
    let isWS = function
        | ' '
        | '\n'
        | '\r' -> true
        | _    -> false

    match str with
    | TokChar ch :: t when isWS ch -> skipWS t
    | _ -> str


let rec private reduceTuple (e: Token) =
    match e with
    | TokTuple (h :: []) -> reduceTuple h
    | _ -> e

let reduceExpression (toks: Token list) =
    match toks with
    | h :: [] -> h
    | _       -> TokExpression toks

let rec private readListOfTokens splitterChar endingChar (boxFunc: Token list -> Token) (acc: Token list) (str: Token list) : Token * Token list =
    let str = skipWS str
    match str with
    | []                                 -> failwith (sprintf "%s doesn't have an end '%c'" ((boxFunc []).TokenTypeName) endingChar)
    | TokChar ch :: t when ch = endingChar -> let exp = (boxFunc (acc |> List.rev)) in exp, t
    | _ ->
        let rec readList (acc: Token list) (str: Token list) : Token * Token list =
            let str = skipWS str
            match str with
            | []                                                        -> failwith (sprintf "%s doesn't have an end '%c'" ((boxFunc []).TokenTypeName) endingChar)
            | TokChar ch :: t when ch = endingChar                      -> let exp = acc |> List.rev |> reduceExpression in exp, str
            | TokChar ch :: t when ch = splitterChar                    ->
                match skipWS t with
                | TokChar ch :: t when ch = endingChar                  -> failwith (sprintf "%s ends with splitter '%c'" ((boxFunc []).TokenTypeName) splitterChar)
                | _     -> let exp = acc |> List.rev |> reduceExpression in exp, t
            | _                                                         -> let tok, nextList = nextToken str in readList (tok :: acc) nextList

        let tok, nextList = readList [] str
        readListOfTokens splitterChar endingChar boxFunc (tok :: acc) nextList

and private readScope   = readListOfTokens ';' '}' TokScope

and private readTuple   = readListOfTokens ',' ')' (TokTuple >> reduceTuple)

and private readList    = readListOfTokens ';' ']' TokList

and private nextToken (str: Token list) : Token * Token list =
    let str = skipWS str
    match str with
    | TokChar '\'' :: t -> readChar        t
    | TokChar '"'  :: t -> readString   [] t // "
    | TokChar '('  :: t -> readTuple    [] t
    | TokChar '{'  :: t -> readScope    [] t
    | TokChar '['  :: t -> readList     [] t
    | TokChar sign :: TokChar ch :: t when (sign = '+' || sign = '-') && isDigit ch -> readNumber str
    | TokChar ch   :: t when isDigit ch   -> readNumber str
    | TokChar ch   :: t when isAlpha ch   -> readSymbolAlphaNum [] str
    | TokChar ch   :: t when isSpecial ch -> readSymbolSpecial  [] str
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
        expList |> TokExpression