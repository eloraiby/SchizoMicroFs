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

//
// This is the schizo expression interpreter (Macro substitution system)
//
module Schizo.Expression

type List<'T>
with
    static member split (count: int) (l: 'T list) =
        let rec loop (n: int, left: 'T list, right: 'T list) =
            match n, right with
            | 0, _  -> left |> List.rev, right
            | n, [] -> left |> List.rev, right
            | n, h :: t -> loop (n - 1, h :: left, t)
        loop (count, [], l)

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
    Source          : string
    Line            : int
    Offset          : int
}

and Environment = {
    ImportedSymbols : Map<string, Exp>
    SymbolMap       : Map<string, Exp>
    UnaryOps        : Map<string, int>
    BinaryOps       : Map<string, int>
}

type Environment
with
    member x.TryFindSymbol s    = x.SymbolMap.TryFind s
    member x.AddSymbol (s, v)   = { x with SymbolMap = x.SymbolMap.Add (s, v) }

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
                        let oh, ot = operands |> List.split (args.Length - 1)
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

