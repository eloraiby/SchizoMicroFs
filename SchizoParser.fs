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

module Schizo.Parser

open Schizo.Expression

type Predicate<'E>  = Predicate of ('E -> bool)
with
    static member (|||) (Predicate a, Predicate b) = Predicate (fun x -> a x || b x)
    static member (&&&) (Predicate a, Predicate b) = Predicate (fun x -> a x && b x)

type Token<'E>      = Token of 'E list
with
    static member from (el: 'E list) = Token (el |> List.rev)

type MatchResult<'E>      =
    | Match     of Token<'E> list * 'E list
    | Unmatched of Token<'E> list * 'E list


let alphaUp =
    function
    | ch when (ch >= 'A' && ch <= 'Z') -> true
    | _ -> false
    |> Predicate

let alphaLow =
    function
    | ch when (ch >= 'a' && ch <= 'z') -> true
    | _ -> false
    |> Predicate

let alpha = alphaUp ||| alphaLow

let digit =
    function
    | ch when  ch >= '0' && ch <= '9'-> true
    | _ -> false
    |> Predicate

let special =
    function
    | '`'   | '!'   | '@'   | '#' 
    | '$'   | '%'   | '^'   | '&' 
    | '*'   | '_'   | '-'   | '+' 
    | '='   | '/'   | '?'   | '<' 
    | '>'   | '|'   | '~'   | '.' -> true
    | _   -> false
    |> Predicate

let oneOrMore (Predicate p) (m: 'T MatchResult) : 'T MatchResult =
    match m with
    | Match (res, stream) ->
        match stream with
        | h :: t when p h ->
            let rec loop (acc: 'T list) (stream: 'T list) =
                match stream with
                | h :: t when p h -> loop (h :: acc) t
                | _               ->  Match (Token.from acc :: res, stream)
            loop [h] t
        | _ -> Unmatched (res, stream)
    | _ -> m

