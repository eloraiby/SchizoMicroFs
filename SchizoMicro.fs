//
// SchizoMicro F# Referemce Compiler
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
open System

open Schizo.Expression
open Schizo.Tokenizer


//
// TODO: use environment, and check if the list contains operators. If it does, reduction
//       should use priorities from the environment to resort the list
//
let reduceList (el: Exp list) : Exp =
    match el with
    | h :: [] -> h
    | EIdentifier s :: t  -> EApplication (EIdentifier s, t)
    | EOperator   s :: t  -> EApplication (EOperator   s, t) // <- This is wrong!
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
    | EChar ch   :: t when isAlpha ch -> readSymbolAlphaNum [] str
    | EChar ch   :: t when isSpecial ch -> readSymbolSpecial [] str
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
        "symbol 1 2 a 3.5 b"
        "\"string\""
        "( )"
        "'\\n'"
        "a b c d"
        "{a b c}"
        "(abc def gfhi)"
        "(abc123 d045ef gf.hi)"
        "(s 123) () 456 a b 123.0 hh !hello $bla%bla()aha::something"
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