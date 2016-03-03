module SchizoVM.Runtime

type SxExpId    = int<exp>
                and [<Measure>] exp

type SxIdentId  = int<idn>
                and [<Measure>] idn

type SxTypeId   = int<ty>
                and [<Measure>] ty

type SxUCaseId  = int<uc>
                and [<Measure>] uc

type SxUnionCase = {
    Name    : string
    Types   : SxTypeId[]
}

type NamedField = {
    Name    : string
    SxType  : SxTypeId
}

type SxType =
    | Unit
    | Bool
    | Int
    | Float
    | Func          of NamedField * SxTypeId
    | Tuple         of SxTypeId[]
    | NamedTuple    of NamedField  []
    | Opaque
    | Union         of SxUnionCase []


// type will be inferred through the symbol table
type SxExp =
    | ExpLet        of SxIdentId * SxExpId
    | ExpFun        of SxIdentId * SxTypeId * SxExpId
    | ExpIf         of SxExpId   * SxExpId  * SxExpId
    | ExpApply      of SxExpId   * SxExpId []

    | ExpBool       of bool
    | ExpInt        of int
    | ExpReal       of float
    | ExpTuple      of SxExpId []
    | ExpNamedTuple of (SxIdentId * SxExpId) []
    | ExpOpaque     of int * SxTypeId    // Assert (run time only (should never exist in code))
    | ExpUnion      of SxTypeId  * SxExpId []
    | ExpSymbol     of SxIdentId   

type SxRuntime = {
    QualifiedTypes  : Map<string, SxType>
    Modules         : Map<string, SxModule>
    TypeTable       : (string * SxType) []
}

and SxModule = {
    Imports : Map<string, SxModule * SxType>
    Exports : Map<string, SxType>
    UCases  : Map<string, SxUnionCase>
    Types   : (string * SxType)  []
    Idents  : (string * SxExp)   []
    Exps    : (SxTypeId * SxExp) []
} with
    member x.Id2Type    (id: SxTypeId)  = let tid = id * 1</ty>  in snd x.Types.[tid]
    member x.TypeName   (id: SxTypeId)  = let tid = id * 1</ty>  in fst x.Types.[tid]
   
    member x.Id2Ident   (id: SxIdentId) = let sid = id * 1</idn> in snd x.Idents.[sid]
    member x.IdentName  (id: SxIdentId) = let sid = id * 1</idn> in fst x.Idents.[sid]
   
    member x.Id2Exp     (id: SxExpId)   = let eid = id * 1</exp> in x.Exps.[eid]

module Engine =
   
    let nothing = "nothing"
