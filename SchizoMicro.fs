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

open Schizo.Expression
open Schizo.Parser

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

//
// floating point grammar
// D   [0-9]
// E   ([Ee][+-]?{D}+)
//
// {D}+"."{D}+{E}?
//
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

    let decimal, nextList =
        match nextList with
        | EChar '.' :: t -> let d, nextList = readInt [] t in EChar '.' :: d, nextList
        | _ -> [], nextList

    let expo, nextList =
        match decimal, nextList with
        | [], _ -> [], nextList
        | _, EChar 'E' :: t
        | _, EChar 'e' :: t ->
            match t with
            | EChar '+' :: t -> let i, nextList = readInt [] t in EChar 'E' :: EChar '+' :: i, nextList
            | EChar '-' :: t -> let i, nextList = readInt [] t in EChar 'E' :: EChar '-' :: i, nextList
            | t -> readInt [] t
        | _, _ -> [], nextList
        

    match integral, decimal, expo with
    | integral, [], _ ->
        let intstr = System.String (integral |> List.toArray |> Array.map (function | EChar ch -> ch | _ -> failwith "unreachable"))

        if intstr.Length <> 0
        then EInt64 (Convert.ToInt64 intstr), nextList
        else failwith "invalid integer number"

    | integral, decimal, [] ->
        let number =  List.append integral decimal 
        let number = System.String (number |> List.toArray |> Array.map (function | EChar ch -> ch | _ -> failwith "unreachable"))

        if number.Length <> 0
        then EReal64 (Convert.ToDouble number), nextList
        else failwith "invalid floating number"

    | integral, decimal, expo ->
        let number = List.append  (List.append integral decimal) expo
        let number = System.String (number |> List.toArray |> Array.map (function | EChar ch -> ch | _ -> failwith "unreachable"))

        if number.Length <> 0
        then EReal64 (Convert.ToDouble number), nextList
        else failwith "invalid floating number"
   
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


let rec readListOfExp splitterChar endingChar (boxFunc: Exp list -> Exp) (acc: Exp list) (str: Exp list) =
    let str = skipWS str
    match str with
    | []                                 -> failwith (sprintf "simplified list doesn't have an end '%c'" endingChar)
    | EChar ch :: t when ch = endingChar -> (boxFunc (acc |> List.rev)), t
    | _ ->
        let rec readList (acc: Exp list) (str: Exp list) : Exp * Exp list =
            let str = skipWS str
            match str with
            | []                                                        -> failwith (sprintf "list doesn't have an end '%c'" endingChar)
            | EChar ch :: t when ch = endingChar                        -> (acc |> List.rev |> reduceList), str
            | EChar ch :: t when ch = splitterChar                      ->
                match skipWS t with
                | EChar ch :: t when ch = endingChar                    -> failwith (sprintf "simplified list ends with splitter '%c'" splitterChar)
                | _     -> (acc |> List.rev |> reduceList), t
            | _                                                         -> let tok, nextList = nextToken str in readList (tok :: acc) nextList

        let tok, nextList = readList [] str
        readListOfExp splitterChar endingChar boxFunc (tok :: acc) nextList


and readSequence    = readListOfExp ';' '}' ESequence

and readTuple       = readListOfExp ',' ')' (ETuple >> reduceTuple)

and readList        = readListOfExp ';' ']' EList

and nextToken (str: Exp list) : Exp * Exp list =
    let str = skipWS str
    match str with
    | EChar '\'' :: t -> readChar          t
    | EChar '"'  :: t -> readString     [] t // "
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

[<EntryPoint>]
let main argv =
    [|
        "'c'"
        "123"
        "0"
        "123.4"
        "123.4e+10"
        "symbol"
        "\"string\""
        "( )"
        "'\\n'"
        "a b c d"
        "{a b c}"
        "(abc def gfhi)"
        "(abc123 d045ef gf.hi)"
        "(s 123) () 456 a b 123.hh !hello $bla%bla()aha"
        "(a b (c 10 12 de) (f 10 11 12))"
        "false"
        "true"
        "d true(false)"
        "hello { a b c d; e f g; 1; 2; 3 }"
        "{ a b c d }"
        "d (1, 2, 3) (1) (ab cd, defg)"
        "e (1, 2, 3) (4, (6, 7)) (ab cd, defg)"
        "[1]"
        "[1; 2; 3; ab cd; (1, 2); (1) ]"
        "[1; 2; 3; ab cd; (1, 2); (1); []]"
    |]
    |> Array.iter
        (fun e ->
            printfn "%A" (parse (e |> Seq.toList |> List.map EChar)))

    0 // return an integer exit code