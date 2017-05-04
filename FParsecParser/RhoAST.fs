module Rholang.AST

type Var = 
    | VarValue of string

and Name = 
    | NameValue of string

and VarPattern =
    | VarPtVar of Var
    | VarPtWild

and CPattern =
    | CPtVar of VarPattern
    | CPtQuote of PPattern

and PatternBind = 
    | PtBind of CPattern * CPattern

and PatternMatchBranchPattern =
    | PtBranch of PPattern * PPattern 

and PPattern =
    | PPtVar of VarPattern
    | PPtNil
    | PPtVal of ValPattern
    | PPtDrop of CPattern
    | PPtInject of CPattern
    | PPtOutput of CPattern * PPattern list
    | PPtInput of PatternBind list * PPattern
    | PPtMatch of PPattern * PatternMatchBranchPattern list
    | PPtNew of VarPattern list * PPattern
    | PPtConstr of Name * PPattern list
    | PPtPar of PPattern list

and ValPattern =
    | VPtStruct of Var * PPattern list

and Value =
    | VQuant of Quantity
    | VEnt of Entity

and Quantity =
    | QInt of int
    | QDouble of float

and Entity = 
    | EChar of char
    | EStruct of Struct
    | ECollect of Collect

and Struct =
    | StructConstr of Var * Proc list

and Proc =
    | PNil
    | PValue of Value
    | PVar of Var
    | PDrop of Chan
    | PInject of Chan
    | PLift of Chan * Proc list
    | PInput of Bind list * Proc
    | PChoice of CBranch list
    | PMatch of Proc * PMBranch list
    | PNew of Var list * Proc
    | PConstr of Name * Proc list
    | PPar of Proc list

and Collect =
    | CString of string

and Bind =
    | InputBind of CPattern * Chan

and Chan =
    | CVar of Var
    | CQuote of Proc

and CBranch =
    | Choice of Bind list * Proc

and PMBranch =
    | PatternMatch of PPattern * Proc

and Contr =
    | DContr of Name * CPattern list * Proc list
