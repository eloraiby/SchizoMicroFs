module Schizo.Logical

open System

type SxUnionCase = {
    Name    : string
    Union   : string // must be union
}

and Field = {
    Name    : string
    Type    : SxType
}

and SxType =
    | Unit
    | Bool
    | Int
    | Float
    | Func          of Field * SxType
    | Tuple         of SxType   []
    | Record        of Field    []
    | Opaque
    | Union
    | UnionCase     of SxUnionCase

// type will be inferred through the symbol table
and SxExpOpCode =
    | ExpLet        of string * SxExp
    | ExpFun        of string * SxType * SxExp
    | ExpIf         of SxExp  * SxExp  * SxExp
    | ExpApply      of SxExp  * SxExp []

    | ExpBool       of bool
    | ExpInt        of int
    | ExpReal       of float
    | ExpTuple      of SxExp []
    | ExpNamedTuple of (string * SxExp) []
    | ExpOpaque     of int * SxType    // Assert (run time only (should never exist in code))
    | ExpData       of SxType  * SxExp []
    | ExpSymbol     of string

and SxExp = {
    OpCode  : SxExp
    Type    : SxType
}


and SxModule = {
    Imports : Map<string, SxModule * SxType>
    Exports : Map<string, SxType>
    UCases  : Map<string, SxUnionCase>
    Types   : (string * SxType) []
    Idents  : (string * SxExp)  []
    Exps    : (SxType * SxExp)  []
}