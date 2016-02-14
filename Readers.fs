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

module Schizo.Readers

open System
open Schizo.Expression

let isAlpha ch =
    if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch = '_')
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
    | '*'   | '-'   | '+'   | '='
    | '/'   | '?'   | '<'   | '>'
    | '|'   | '~'   | '.'   | ':' -> true
    | _   -> false

let readChar (env: Environment) (str: Exp list) : Environment * Exp * Exp list =
    match str with
    | EChar '\\' :: EChar ch :: EChar '\'' :: t ->
        match ch with
        | 'n'  -> env, EChar '\n', t
        | 'r'  -> env, EChar '\r', t
        | 'a'  -> env, EChar '\a', t
        | '\'' -> env, EChar '\'', t
        | _    -> failwith (sprintf "unknown escape character %c after \\ in char" ch)
    | EChar ch   :: EChar '\'' :: t       -> env, EChar ch, t
    | _               -> failwith "not a well formed char"

let rec readString (env: Environment) (acc: Exp list) (str: Exp list) : Environment * Exp * Exp list =
    match str with
    | EChar '"'  :: t -> env, EList (acc |> List.rev), t // "
    | EChar '\\' :: EChar ch :: t ->
        let newCh =
            match ch with
            | 'n'  -> '\n'
            | 'r'  -> '\r'
            | 'a'  -> '\a'
            | '\'' -> '\''
            | _    -> failwith (sprintf "unknown escape character %c after \\ in string" ch)
        readString env (EChar newCh :: acc) t
    | ch :: t -> readString env (ch :: acc) t
    | _ -> failwith "ill formated string"

let rec readSymbolAlphaNum (env: Environment) (acc: Exp list) (str: Exp list) : Environment * Exp * Exp list =
    match str with
    | EChar ch :: t when isAlpha ch || isDigit ch -> readSymbolAlphaNum env (EChar ch :: acc) t
    | _ ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | EChar ch -> ch | _ -> failwith "unreachable"))
        let sym =
            match sym with
            | "true" -> EBoolean true
            | "false" -> EBoolean false
            | _ -> EIdentifier sym
        env, sym, str    // anything else, just bail

let rec readSymbolSpecial (env: Environment) (acc: Exp list) (str: Exp list) : Environment * Exp * Exp list =
    match str with
    | EChar ch :: t when isSpecial ch -> readSymbolSpecial env (EChar ch :: acc) t
    | _                     ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | EChar ch -> ch | _ -> failwith "unreachable"))
        env, EOperator sym, str    // anything else, just bail
//
// floating point grammar
// D   [0-9]
// E   ([Ee][+-]?{D}+)
//
// {D}+"."{D}+{E}?
//
let readNumber (env: Environment) (str: Exp list) : Environment * Exp * Exp list=
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
        

    let token, nextList =
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

    match nextList with
    | EChar ch :: l when isAlpha ch -> failwith "invalid number: followed by a alpha or special character, separator or space is needed"
    | _ -> env, token, nextList
   
let rec skipWS (str: Exp list) : Exp list =
    let isWS = function
        | ' '
        | '\n'
        | '\r' -> true
        | _    -> false

    match str with
    | EChar ch :: t when isWS ch -> skipWS t
    | _ -> str
