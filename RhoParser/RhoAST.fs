module Rholang.AST

type Var = 
    | VarValue of string

and Name = 
    | NameValue of string

and ValPattern =
    | VPtStruct of Var * PPattern list
    | VPtTuple of PPattern list
    | VPtTrue
    | VPtFalse
    | VPtInt of int
    | VPtDbl of float
    | VPtStr of string

and PatternPatternMatch =
    | PtBranch of PPattern * PPattern 

and PatternBind = 
    | PtBind of CPattern * CPattern

and CPattern =
    | CPtVar of VarPattern
    | CPtQuote of PPattern
    | CValPtrn of ValPattern

and PPattern =
    | PPtVar of VarPattern
    | PPtNil
    | PPtVal of ValPattern
    | PPtDrop of CPattern
    | PPtInject of CPattern
    | PPtOutput of CPattern * PPattern list
    | PPtInput of PatternBind list * PPattern
    | PPtMatch of PPattern * PatternPatternMatch list
    | PPtNew of VarPattern list * PPattern
    | PPtConstr of Name * PPattern list
    | PPtPar of PPattern list

and VarPattern =
    | VarPtVar of Var
    | VarPtWild

and Collect =
    | CString of string

and Struct =
    | StructConstr of Var * Proc list

and Entity = 
    | EChar of char
    | EStruct of Struct
    | ECollect of Collect
    | ETuple of Proc list

and RhoBool =
    | QTrue
    | QFalse

and Quantity =
    | QBool of RhoBool
    | QInt of int
    | QDouble of float

and Value =
    | VQuant of Quantity
    | VEnt of Entity

and CBranch =
    | Choice of Bind list * Proc

and PMBranch =
    | PatternMatch of PPattern * Proc

and Bind =
    | InputBind of CPattern * Chan
    | CondInputBind of CPattern * Chan * Proc

and Chan =
    | CVar of Var
    | CQuote of Proc

and Proc =
    | PNil
    | PValue of Value
    | PVar of Var
    | PDrop of Chan
    | PInject of Chan
    | PLift of Chan * Proc list
    | PFoldL of Bind * Bind * Proc
    | PFoldR of Bind * Bind * Proc
    | PInput of Bind list * Proc
    | PChoice of CBranch list
    | PMatch of Proc * PMBranch list
    | PNew of Var list * Proc
    | PConstr of Name * Proc list
    | PPar of Proc list

and Contr =
    | DContr of Name * CPattern list * Proc

