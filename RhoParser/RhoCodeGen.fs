module Rholang.CodeGen

open Rholang.AST

type internal CodeWriter = string -> unit

let internal ws = " "
let internal nl = "\n"

type CodeGenerator =
    static member Generate(n:Rholang.AST.Var, cw:CodeWriter) =
        match n with
        | Var.VarValue(v) -> cw v

    static member Generate(nl:Rholang.AST.Var list, cw:CodeWriter) =
        // separator nonempty Var "," ;
        let rec f (l:Var list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw ", "
                f r
        f nl

    static member Generate(n:Rholang.AST.Name, cw:CodeWriter) =
        match n with
        | Name.NameValue(v) -> cw v

    static member Generate(n:Rholang.AST.ValPattern, cw:CodeWriter) =
        // VPtStruct. ValPattern ::= Var "{" [PPattern] "}" ;
        match n with
        | ValPattern.VPtStruct(v, pl) -> 
            CodeGenerator.Generate(v, cw)
            cw " { "
            CodeGenerator.Generate(pl, cw)
            cw " }"
            cw nl

    static member Generate(n:PatternPatternMatch, cw:CodeWriter) =
        // PtBranch. PatternPatternMatch ::= PPattern "=>" "{" PPattern "}" ;
        match n with
        | PatternPatternMatch.PtBranch(p1, p2) ->
            CodeGenerator.Generate(p1, cw)
            cw " => {"
            CodeGenerator.Generate(p2, cw)
            cw "}"
            cw nl

    static member Generate(nl:PatternPatternMatch list, cw:CodeWriter) =
        // separator nonempty PatternPatternMatch "" ;
        let rec f (l:PatternPatternMatch list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw "; "
                f r
        f nl

    static member Generate(n:PatternBind, cw:CodeWriter) =
        // PtBind.   PatternBind ::= CPattern "<-" CPattern ;
        match n with
        | PatternBind.PtBind(cp1, cp2) ->
            CodeGenerator.Generate(cp1, cw)
            cw " <- "
            CodeGenerator.Generate(cp2, cw)

    static member Generate(nl:PatternBind list, cw:CodeWriter) =
        // separator nonempty PatternBind ";" ;
        let rec f (l:PatternBind list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw "; "
                f r
        f nl
        
    static member Generate(n:CPattern, cw:CodeWriter) =
        // CPtVar.    CPattern ::= VarPattern ;
        // CPtQuote.  CPattern ::= "@" PPattern3 ;
        match n with
        | CPattern.CPtVar(vp) ->
            CodeGenerator.Generate(vp, cw)
        | CPattern.CPtQuote(pp) ->
            cw "@"
            CodeGenerator.Generate(pp, cw) 

    static member Generate(nl:CPattern list, cw:CodeWriter) =
        // separator CPattern "," ;
        let rec f (l:CPattern list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw ", "
                f r
        f nl

    static member Generate(n:PPattern, cw:CodeWriter) =
        match n with
        | PPattern.PPtVar(vp) -> 
            //PPtVar.    PPattern4 ::= VarPattern ;
            CodeGenerator.Generate(vp, cw)
        | PPattern.PPtNil ->
            //PPtNil.    PPattern4 ::= "Nil" ;
            cw "Nil"
        | PPattern.PPtVal(vp) ->
            //PPtVal.    PPattern4 ::= ValPattern ;
            CodeGenerator.Generate(vp, cw)
        | PPattern.PPtDrop(cp) ->
            //PPtDrop.   PPattern3 ::= "*" CPattern ;
            cw "*"
            CodeGenerator.Generate(cp, cw)
        | PPattern.PPtInject(cp) ->
            //PPtInject. PPattern3 ::= "#" CPattern ;
            cw "#"
            CodeGenerator.Generate(cp, cw)
        | PPattern.PPtOutput(cp, ppl) ->
            //PPtOutput. PPattern2 ::= CPattern "!" "(" [PPattern] ")" ;
            CodeGenerator.Generate(cp, cw)
            cw "! ("
            CodeGenerator.Generate(ppl, cw)
            cw ")"
        | PPattern.PPtInput(pbl, pp) ->
            //PPtInput.  PPattern1 ::= "for" "(" [PatternBind] ")" "{" PPattern "}" ;
            cw "for ("
            CodeGenerator.Generate(pbl, cw)
            cw ") {"
            CodeGenerator.Generate(pp, cw)
            cw "}"
            cw nl
        | PPattern.PPtMatch(pp, ppml) ->
            //PPtMatch.  PPattern1 ::= "match" PPattern "with" [PatternPatternMatch] ;
            cw "match "
            CodeGenerator.Generate(pp, cw)
            cw " with "
            CodeGenerator.Generate(ppml, cw)
            cw nl
        | PPattern.PPtNew(vpl, pp) ->
            //PPtNew.    PPattern1 ::= "new" [VarPattern] "in" PPattern1 ;
            cw "new "
            CodeGenerator.Generate(vpl, cw)
            cw " in "
            CodeGenerator.Generate(pp, cw)
            cw nl
        | PPattern.PPtConstr(n, ppl) ->
            //PPtConstr. PPattern1 ::= Name "(" [PPattern] ")" ;
            CodeGenerator.Generate(n, cw)
            cw " ("
            CodeGenerator.Generate(ppl, cw)
            cw ")"
            cw nl
        | PPattern.PPtPar(ppl) ->
            //PPtPar.    PPattern  ::= PPattern "|" PPattern1 ;
            let rec f (l:PPattern list) =
                match l with
                | [] -> ()
                | [p] -> CodeGenerator.Generate(p, cw)
                | p :: r -> 
                    CodeGenerator.Generate(p, cw)
                    cw " | "
                    f r

            f ppl

    static member Generate(nl:Rholang.AST.PPattern list, cw:CodeWriter) =
        // separator PPattern "," ;
        let rec f (l:PPattern list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw ", "
                f r
        f nl

    static member Generate(n:Rholang.AST.VarPattern, cw:CodeWriter) =
        match n with
        | VarPattern.VarPtVar(v) -> 
            //VarPtVar.  VarPattern ::= Var ;
            CodeGenerator.Generate(v, cw)
        | VarPattern.VarPtWild   -> 
            //VarPtWild. VarPattern ::= "_" ;
            cw "_"

    static member Generate(nl:Rholang.AST.VarPattern list, cw:CodeWriter) =
        // separator VarPattern "," ;
        let rec f (l:VarPattern list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw ", "
                f r
        f nl

    static member Generate(n:Rholang.AST.Collect, cw:CodeWriter) =
        // CString. Collect ::= String ;
        match n with 
        | Collect.CString(s) -> 
            cw "\""
            cw (s.Replace("\"", "\\\""))
            cw "\""

    static member Generate(n:Rholang.AST.Struct, cw:CodeWriter) =
        // StructConstr. Struct ::= Var "{" [Proc] "}" ;
        match n with
        | Struct.StructConstr(v, pl) ->
            CodeGenerator.Generate(v, cw)
            cw " { "
            CodeGenerator.Generate(pl, cw)
            cw "}"
            cw nl

    static member Generate(n:Rholang.AST.Entity, cw:CodeWriter) =
        match n with 
        | Entity.EChar(c) ->
            //EChar.    Entity   ::= Char ;
            cw "\'"
            cw (c.ToString())
            cw "\'"
        | Entity.EStruct(s) ->
            //EStruct.  Entity   ::= Struct ;
            CodeGenerator.Generate(s, cw)
        | Entity.ECollect(c) ->
            //ECollect. Entity   ::= Collect ;
            CodeGenerator.Generate(c, cw)

    static member Generate(n:Rholang.AST.Quantity, cw:CodeWriter) =
        match n with 
        | Quantity.QInt(i) -> 
            //QInt.     Quantity ::= Integer ;
            cw (i.ToString())
        | Quantity.QDouble(d) ->
            //QDouble.  Quantity ::= Double ;
            cw (d.ToString())

    static member Generate(n:Rholang.AST.Value, cw:CodeWriter) =
        match n with
        | Value.VQuant(q) ->
            //VQuant.   Value    ::= Quantity ;
            CodeGenerator.Generate(q, cw)
        | Value.VEnt(e) ->
            //VEnt.     Value    ::= Entity ;
            CodeGenerator.Generate(e, cw)

    static member Generate(n:Rholang.AST.CBranch, cw:CodeWriter) =
        match n with
        | CBranch.Choice(bl, p) ->
            // Choice. CBranch ::= "case" [Bind] "=>" "{" Proc "}" ;
            cw "case "
            CodeGenerator.Generate(bl, cw)
            cw "=> {"
            CodeGenerator.Generate(p, cw)
            cw "}"
            cw nl

    static member Generate(nl:Rholang.AST.CBranch list, cw:CodeWriter) =
        // separator nonempty CBranch "" ;
        let rec f (l:CBranch list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw " "
                f r
        f nl    

    static member Generate(n:Rholang.AST.PMBranch, cw:CodeWriter) =
        // PatternMatch. PMBranch ::= PPattern "=>" "{" Proc "}" ;
        match n with
        | PMBranch.PatternMatch(pp, p) ->
            CodeGenerator.Generate(pp, cw)
            cw " => {"
            CodeGenerator.Generate(p, cw)
            cw "}"
            cw nl

    static member Generate(nl:Rholang.AST.PMBranch list, cw:CodeWriter) =
        // separator nonempty PMBranch "" ; 
        let rec f (l:PMBranch list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw " "
                f r
        f nl    

    static member Generate(n:Rholang.AST.Bind, cw:CodeWriter) =
        // InputBind. Bind ::= CPattern "<-" Chan ;
        match n with 
        | Bind.InputBind(cp, c) ->
            CodeGenerator.Generate(cp, cw)
            cw " <- "
            CodeGenerator.Generate(c, cw)

    static member Generate(nl:Rholang.AST.Bind list, cw:CodeWriter) =
        // separator nonempty Bind ";" ;
        let rec f (l:Bind list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw "; "
                f r
        f nl

    static member Generate(n:Rholang.AST.Chan, cw:CodeWriter) =
        match n with
        | Chan.CVar(v) ->
            //CVar.    Chan ::= Var ;
            CodeGenerator.Generate(v, cw)
        | Chan.CQuote(p) ->
            //CQuote.  Chan ::= "@" Proc3 ;
            CodeGenerator.Generate(p, cw)
            
    static member Generate(n:Rholang.AST.Proc, cw:CodeWriter) =
        match n with
        | Proc.PNil ->
            //PNil.    Proc4 ::= "Nil" ;
            cw "Nil"
        | Proc.PValue(v) -> 
            //PValue.  Proc4 ::= Value ;
            CodeGenerator.Generate(v, cw)
        | Proc.PVar(v) ->
            //PVar.    Proc4 ::= Var ;
            CodeGenerator.Generate(v, cw)
        | Proc.PDrop(c) ->
            //PDrop.   Proc3 ::= "*" Chan ;
            cw "*"
            CodeGenerator.Generate(c, cw)
        | Proc.PInject(c) ->
            //PInject. Proc3 ::= "#" Chan ;
            cw "#"
            CodeGenerator.Generate(c, cw)
        | Proc.PLift(c, pl) ->
            //PLift.   Proc2 ::= Chan "!" "(" [Proc] ")" ;
            CodeGenerator.Generate(c, cw)
            cw "! ("
            CodeGenerator.Generate(pl, cw)
            cw ")"
            cw nl
        | Proc.PInput(bl, p) ->
            //PInput.  Proc1 ::= "for" "(" [Bind] ")" "{" Proc "}" ;
            cw "for ("
            CodeGenerator.Generate(bl, cw)
            cw ") {"
            CodeGenerator.Generate(p, cw)
            cw "}"
            cw nl
        | Proc.PChoice(cbl) ->
            //PChoice. Proc1 ::= "select" "{" [CBranch] "}" ;
            cw "select {"
            CodeGenerator.Generate(cbl, cw)
            cw "}"
            cw nl
        | Proc.PMatch(p, pmbl) ->
            //PMatch.  Proc1 ::= "match" Proc "with" [PMBranch] ;
            cw "match "
            CodeGenerator.Generate(p, cw)
            cw " with "
            CodeGenerator.Generate(pmbl, cw)
            cw nl
        | Proc.PNew(vl, p) ->
            //PNew.    Proc1 ::= "new" [Var] "in" Proc1 ;
            cw "new "
            CodeGenerator.Generate(vl, cw)
            cw " in "
            CodeGenerator.Generate(p, cw)
            cw nl
        | Proc.PConstr(n, pl) ->
            //PConstr. Proc1 ::= Name "(" [Proc] ")" ;
            CodeGenerator.Generate(n, cw)
            cw " ("
            CodeGenerator.Generate(pl, cw)
            cw ")"
            cw nl
        | Proc.PPar(pl) ->
            //PPar.    Proc  ::= Proc "|" Proc1 ;
            CodeGenerator.Generate(pl, cw)

    static member Generate(nl:Rholang.AST.Proc list, cw:CodeWriter) =
        // separator nonempty Proc "," ;
        let rec f (l:Proc list) =
            match l with
            | [] -> ()
            | [p] -> CodeGenerator.Generate(p, cw)
            | p :: r -> 
                CodeGenerator.Generate(p, cw)
                cw "; "
                f r
        f nl
        
    static member Generate(n:Rholang.AST.Contr, cw:CodeWriter) =
        // DContr. Contr ::= "contract" Name "(" [CPatterna] ")" "=" "{" Proc "}" ;
        match n with 
        | Contr.DContr(n, cpl, p) ->
            cw "contract "
            CodeGenerator.Generate(n, cw)
            cw " ("
            CodeGenerator.Generate(cpl, cw)
            cw ") = {"
            CodeGenerator.Generate(p, cw)
            cw "}"
            cw nl


let GenerateRholang (ast:Rholang.AST.Contr) =
    use sw = new System.IO.StringWriter()

    let cw (s:string) =
        sw.Write(s)

    CodeGenerator.Generate(ast, cw)
    sw.Flush()
    (sw.ToString())
    
