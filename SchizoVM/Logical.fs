module Schizo.Logical

open System

type FuncType =
    | Native
    | Interpreted

type SxUnionCase = {
    Name    : string
    Union   : SxType // must be union (= Union s)
    Type    : SxType
}

and Field = {
    Name    : string
    Type    : SxType
}

and [<CustomEqualityAttribute; CustomComparisonAttribute>] SxType =
    | Unit
    | Bool
    | Int
    | Float
    | Func          of Field * SxType
    | Tuple         of SxType []
    | Record        of Field  []
    | Opaque        of string
    | Union         of string
    | UnionCase     of SxUnionCase
with
    override x.Equals(y: obj) =
        let y = unbox<SxType> y
        match x, y with
        | Unit          , Unit      -> true    
        | Bool          , Bool      -> true    
        | Int           , Int       -> true    
        | Float         , Float     -> true    
        | Func (fx, tx) , Func (fy, ty) -> fx = fy && tx = ty
        | Tuple txs     , Tuple tys -> txs = tys
        | Record rx     , Record ry -> rx = ry
        | Opaque sx     , Opaque sy -> sx = sy
        | Union ux      , Union uy  -> ux = uy
        | UnionCase ucx , UnionCase ucy -> ucx = ucy
        | _                         -> false

    override x.GetHashCode () =
        match x with
        | Unit          -> "Unit".GetHashCode()
        | Bool          -> "Bool".GetHashCode()
        | Int           -> "Int".GetHashCode()
        | Float         -> "Float".GetHashCode()
        | Func   (f, t) -> (f.GetHashCode()) ^^^ (t.GetHashCode())
        | Tuple      ts -> ts.GetHashCode()
        | Record     fs -> fs.GetHashCode()
        | Opaque      o -> o.GetHashCode()
        | Union       u -> u.GetHashCode()
        | UnionCase ucs -> ucs.GetHashCode()

    interface IComparable with
        member x.CompareTo (y: obj) =
            let y = unbox<SxType> y
            x.GetHashCode().CompareTo (y.GetHashCode())
               

// type will be inferred through the symbol table
and SxExp =
    | ExpLet        of string * SxExp
    | ExpFun        of string * SxType * SxExp
    | ExpIf         of SxExp  * SxExp  * SxExp
    | ExpApply      of SxExp  * SxExp []
    | ExpGetField   of string * SxType * SxExp

    | ExpUnit
    | ExpBool       of bool
    | ExpInt        of int
    | ExpReal       of float
    | ExpTuple      of SxExp []
    | ExpRecord     of (string * SxExp) []
    | ExpOpaque     of int * SxType    // Assert (runtime only (should never exist in code))
    | ExpData       of SxType  * SxExp []
    | ExpSymbol     of string

[<CustomEqualityAttribute; CustomComparisonAttribute>] 
type SxModule = {
    Name    : string
    Hash    : string
    Imports : Map<SxModule * string, SxType>
    Exports : Map<string, SxType>
    Types   : Map<string, SxType>
    Exps    : (SxType * SxExp)  []
} with
    override x.Equals(y: obj) =
        let y = unbox<SxModule> y
        x.Name = y.Name && x.Hash = y.Hash

    override x.GetHashCode () = x.Hash.GetHashCode()

    interface IComparable with
        member x.CompareTo (y: obj) =
            let y = unbox<SxModule> y
            in x.Name.CompareTo y.Name

type Environment = {
    Modules : Map<string, SxModule>
}

