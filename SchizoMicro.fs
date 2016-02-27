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
module Main
open System

open Schizo.Expression
open Schizo.Tokens
(*
open Schizo.Readers

let infixMacro (env: Environment) (el: Exp list) : Exp =
    match el with
    | EInt64 priority :: EOperator op :: [] -> Environment ({ env with BinaryOps = env.BinaryOps.Add(op, priority |> int) })
    | _ -> failwith "infix operator requires an operator and int"

let moduleMacro (env: Environment) (el: Exp list) : Exp =
    match el with
    | EIdentifier ident :: ESequence seqList :: [] -> failwith "not implemented"  
    | _ -> failwith "module requires an identifier and a sequence"

let rec splitExpList (env: Environment) (el: Exp list) : Environment * Exp list =
    let reduceToExp x =
        match x with
        | h :: [] -> h
        | l       -> let _, t = l |> List.rev |> listToApp env in t

    let temp, state =
        el
        |> List.fold(fun (temp, state) el ->
            match el with
            | EOperator op ->
                match env.BinaryOps.TryFind op with
                | Some _ -> [], (EOperator op) :: (reduceToExp temp) :: state
                | _ -> el :: temp, state
            | _ -> el :: temp, state) ([], [])

    let expList = (reduceToExp temp) :: state

    env, expList |> List.rev
     
and isBinOpExpList (env: Environment) (el: Exp list) : bool =
    el
    |> List.filter(
        function
        | EOperator op when env.BinaryOps.TryFind op |> Option.isSome -> true
        | EOperator op when env.UnaryOps.TryFind op  |> Option.isSome -> false  // unary operators are discarded
        | EOperator op -> failwith (sprintf "operator %s is not defined as binary or unary operator!" op)
        | _ -> false)
    |> List.length
    |> (<>) 0

and listToApp (env: Environment) (el: Exp list) : Environment * Exp =
    match el with
    |                   h :: [] -> env, h
    | EIdentifier       s :: t  -> env, EApplication (EIdentifier s, t)
    | EApplication (h, t) :: tl -> env, EApplication (EApplication (h, t), tl)
    | _ -> failwith "Expression list is not an application"

and reduceList (env: Environment) (el: Exp list) : Environment * Exp =
    // check if the expression has a binary operator
    match isBinOpExpList env el with
    | true ->
        let env, expList = splitExpList env el
        expList
        |> reduceOperator env
    | false -> el |> listToApp env

and reduceOperator (env: Environment) (el: Exp list) : Environment * Exp =
    match el with
    | argL :: EOperator op :: argR :: [] when Option.isSome (env.BinaryOps.TryFind op)-> env, EApplication (EOperator op, argL :: argR :: [])
    | argL :: EOperator opR :: argR :: EOperator opL :: t -> 
        if env.BinaryOps.[opR] < env.BinaryOps.[opL]
        then reduceOperator env ((EApplication (EOperator opR, argL :: argR :: [])) :: EOperator opL :: t)
        else
            let env, expList = reduceOperator env (argR :: EOperator opL :: t)
            env, EApplication(EOperator opR, argL :: expList :: [])
    | _ -> failwith "not a binary operator expression"

*)

[<EntryPoint>]
let main argv =
    let env = Environment.empty
    let env = { env with BinaryOps = env.BinaryOps.Add(":",  -1) }
    let env = { env with BinaryOps = env.BinaryOps.Add("->", -2) }
    let env = { env with BinaryOps = env.BinaryOps.Add(".",  -3) }
    let env = { env with BinaryOps = env.BinaryOps.Add("*",   1) }
    let env = { env with BinaryOps = env.BinaryOps.Add("/",   1) }
    let env = { env with BinaryOps = env.BinaryOps.Add("+",   2) }
    let env = { env with BinaryOps = env.BinaryOps.Add("-",   2) }
    let env = { env with BinaryOps = env.BinaryOps.Add("|",   3) }
    let env = { env with BinaryOps = env.BinaryOps.Add("-->", 3) }
    let env = { env with BinaryOps = env.BinaryOps.Add("$",   7) }

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
        "(s 123) () 456 a b 123.0 hh !hello $bla%bla()aha:something"
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
        "module A { funA : a -> b -> c; funB : d }"
        "1 + 2"
        "1 + 2 * 5"
        "1 - 10 / 30 * 50"
        "hello world + 10 / 20 - 20 * 50 | abc d e fg hi 12 --> right"
        "[1; 2; 3; 4]"
        "{ a b; c }"
        "module Type {}"
    |]
    |> Array.iter
        (fun e ->
            let el =
                e
                |> Seq.toList
                |> List.fold
                    (fun (di: DebugInfo, acc) ch ->
                        match ch with
                        | '\n' -> let di = { di with Line = di.Line + 1; Offset = di.Offset + 1 } in (di, TokChar (di, ch) :: acc)
                        | _    -> let di = { di with Offset = di.Offset + 1 } in (di, TokChar (di, ch) :: acc)) (DebugInfo.empty, [])
                |> snd
                |> List.rev

            let el = Token.tokenize el
            printfn "%O" el)

    0 // return an integer exit code