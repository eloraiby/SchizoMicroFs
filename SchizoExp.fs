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

//
// This is the schizo expression interpreter (Macro substitution system)
//
module Schizo.Expression

open Schizo.Tokens

type List<'T>
with
    static member splitAt (count: int) (l: 'T list) =
        let rec loop (n: int, left: 'T list, right: 'T list) =
            match n, right with
            | 0, _  -> left |> List.rev, right
            | n, [] -> left |> List.rev, right // TODO: this makes split very permissive and will not garantie n elements
            | n, h :: t -> loop (n - 1, h :: left, t)
        loop (count, [], l)

and Environment = {
    Parent          : Environment option
    Modules         : Environment list
    SymbolMap       : Map<string, Exp * SxType>
    UnaryOps        : Map<string, int * SxType>
    BinaryOps       : Map<string, int * SxType>
}

type Environment
with
    member x.TryFindSymbol s    =
        let sym = x.SymbolMap.TryFind s
        match sym with
        | None ->
            match x.Parent with
            | None -> None
            | Some p -> p.TryFindSymbol s
        | _ -> sym

    member x.AddSymbol (s, v)   = { x with SymbolMap = x.SymbolMap.Add (s, v) }

    static member empty = {
        Parent          = None
        Modules         = []
        SymbolMap       = Map.empty
        UnaryOps        = Map.empty
        BinaryOps       = Map.empty
        }

(*
type Exp
with
    static member fromString(str: string) = str |> Seq.toList |> List.map EChar |> EList
    member x.Eval (env: Environment) =
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
            | EList     _   -> c
            | EIdentifier s ->
                match env.TryFindSymbol s with
                | Some c -> evalCell env c
                | None   -> c   // no binding found, return the symbol as is

            | EOperator s ->
                match env.TryFindSymbol s with
                | Some c -> evalCell env c
                | None   -> c   // no binding found, return the symbol as is

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
        evalCell env x
*)