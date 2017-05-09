module RhoProgramGenerator

open Rholang.AST

let GenerateList<'T when 'T : equality> (state: string list) (f: string list -> 'T seq) (nonempty: bool) =
    let statename = (typeof<'T>).Name
    let f1() = f ((statename + "-1") :: state)
    let f2a() = f ((statename + "-2a") :: state)
    let f2b() = f ((statename + "-2b") :: state)

    seq {
        // Generate empty list
        if (not nonempty) then
            yield []

        // Generate list with one element
        for x in (f1()) do
            yield [ x; ]

        // Generate list with two elements
        (*
        for x1 in (f2a()) do
            for x2 in (f2b()) do
                if x1 <> x2 then
                    yield [ x1; x2; ]
        *)
    }


let rec GenerateName (state:string list) : Name seq =
    // token Name (upper (letter | digit | '_' | '\'')*) ;
    seq {
        yield Name.NameValue("N")
        yield Name.NameValue("Name_1")
    }

and GenerateVar (state:string list) : Var seq =
    // token Var (lower (letter | digit | '_' | '\'')*) ;
    seq {
        yield Var.VarValue("v")
        yield Var.VarValue("var_1")
    }

and GenerateVarList (state:string list) : Var list seq =
    // separator nonempty Var "," ;
    GenerateList<Var> state GenerateVar true

and GenerateValPattern (state:string list) : ValPattern seq =
    // VPtStruct. ValPattern ::= Var "{" [PPattern] "}" ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            for v in (GenerateVar ("ValPattern" :: state)) do
                for pp in (GeneratePPatternList ("ValPattern.VPtStruct" :: state)) do
                    yield ValPattern.VPtStruct(v, pp)
        }

and GeneratePatternPatternMatch (state:string list) : PatternPatternMatch seq =
    // PtBranch. PatternPatternMatch ::= PPattern "=>" "{" PPattern "}" ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            for pp1 in (GeneratePPattern 0 ("PatternPatternMatch.PtBranch-p1" :: state)) do
                for pp2 in (GeneratePPattern 0 ("PatternPatternMatch.PtBranch-p2" :: state)) do
                    yield PatternPatternMatch.PtBranch(pp1, pp2)
        }

and GeneratePatternPatternMatchList (state:string list) : PatternPatternMatch list seq =
    // separator nonempty PatternPatternMatch "" ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<PatternPatternMatch> state GeneratePatternPatternMatch true

and GeneratePatternBind (state:string list) : PatternBind seq =
    //-- Bind pattern
    //PtBind.   PatternBind ::= CPattern "<-" CPattern ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            for cp1 in (GenerateCPattern ("PatternBind.PtBind-1" :: state)) do
                for cp2 in (GenerateCPattern ("PatternBind.PtBind-2" :: state)) do
                    yield PatternBind.PtBind(cp1, cp2)
        }

and GeneratePatternBindList (state:string list) : PatternBind list seq =
    //separator nonempty PatternBind ";" ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<PatternBind> state GeneratePatternBind true

and GenerateCPattern (state:string list) : CPattern seq =
    // -- Channel patterns
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            // CPtVar.    CPattern ::= VarPattern ;
            for vp in (GenerateVarPattern ("CPatternCPtVar" :: state)) do
                yield CPattern.CPtVar(vp)
        
            // CPtQuote.  CPattern ::= "@" PPattern3 ;
            for pp3 in (GeneratePPattern 3 ("CPtQuote" :: state)) do
                yield CPattern.CPtQuote(pp3)
        }

and GenerateCPatternList (state:string list) : CPattern list seq =
    //separator CPattern "," ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<CPattern> state GenerateCPattern false

and GeneratePPattern (n:int) (state:string list) : PPattern seq =
    //-- Process patterns
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            if (n = 0 || n = 4) then
                //PPtVar.    PPattern4 ::= VarPattern ;
                for vp in (GenerateVarPattern ("PPattern.PPtVar" :: state)) do
                    yield PPattern.PPtVar(vp)

                //PPtNil.    PPattern4 ::= "Nil" ;
                yield PPattern.PPtNil

                //PPtVal.    PPattern4 ::= ValPattern ;
                for vp in (GenerateValPattern ("PPattern.PPtVal" :: state)) do
                    yield PPattern.PPtVal(vp)

            if (n = 0 || n = 3) then
                //PPtDrop.   PPattern3 ::= "*" CPattern ;
                for cp in (GenerateCPattern ("PPattern.PPtDrop" :: state)) do
                    yield PPattern.PPtDrop(cp)

                //PPtInject. PPattern3 ::= "#" CPattern ;
                for cp in (GenerateCPattern ("PPattern.PPtInject" :: state)) do
                    yield PPattern.PPtInject(cp)

            if (n = 0 || n = 2) then
                //PPtOutput. PPattern2 ::= CPattern "!" "(" [PPattern] ")" ;
                for cp in (GenerateCPattern ("PPattern.PPtOutput" :: state)) do
                    for ppl in (GeneratePPatternList ("PPattern.PPtOutput" :: state)) do
                        yield PPattern.PPtOutput(cp, ppl)

            if (n = 0 || n = 1) then
                //PPtInput.  PPattern1 ::= "for" "(" [PatternBind] ")" "{" PPattern "}" ;
                for pbl in (GeneratePatternBindList ("PPattern.PPtInput" :: state)) do
                    for pp in (GeneratePPattern 0 ("PPattern.PPtInput" :: state)) do
                        yield PPattern.PPtInput(pbl, pp)

                //PPtMatch.  PPattern1 ::= "match" PPattern "with" [PatternPatternMatch] ;
                for pp in (GeneratePPattern 0 ("PPattern.PPtMatch" :: state)) do
                    for ppml in (GeneratePatternPatternMatchList ("PPattern.PPtMatch" :: state)) do
                        yield PPattern.PPtMatch(pp, ppml)

                //PPtNew.    PPattern1 ::= "new" [VarPattern] "in" PPattern1 ;
                for vpl in (GenerateVarPatternList ("PPattern.PPtNew" :: state)) do
                    for pp1 in (GeneratePPattern 1 ("PPattern.PPtNew" :: state)) do
                        yield PPattern.PPtNew(vpl, pp1)

                //PPtConstr. PPattern1 ::= Name "(" [PPattern] ")" ;
                for name in (GenerateName ("PPattern.PPtConstr" :: state)) do
                    for ppl in (GeneratePPatternList ("PPattern.PPtConstr" :: state)) do
                        yield PPattern.PPtConstr(name, ppl)

            //PPtPar.    PPattern  ::= PPattern "|" PPattern1 ;
            for ppl in (GenerateList<PPattern> ("PPattern.PPtPar" :: state) (GeneratePPattern 0) true) do
                yield PPattern.PPtPar(ppl)
        }

and GeneratePPatternList (state:string list) : PPattern list seq =
    // separator PPattern "," ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<PPattern> state (GeneratePPattern 0) false

and GenerateVarPattern (state:string list) : VarPattern seq =
    //-- Variable patterns
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            //VarPtVar.  VarPattern ::= Var ;
            for v in (GenerateVar ("VarPtVar" :: state)) do
                yield VarPattern.VarPtVar(v)
        
            //VarPtWild. VarPattern ::= "_" ;
            yield VarPattern.VarPtWild
        }

and GenerateVarPatternList (state:string list) : VarPattern list seq =
    //separator VarPattern "," ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<VarPattern> state GenerateVarPattern false

and GenerateCollect (state:string list) : Collect seq =
    //CString. Collect ::= String ;
    seq {
        yield Collect.CString("CString")
        yield Collect.CString("CString \"with quote\"")
    }

and GenerateStruct (state:string list) : Struct seq =
    // StructConstr. Struct ::= Var "{" [Proc] "}" ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            for v in (GenerateVar ("Struct.StructConstr" :: state)) do
                for pl in (GenerateProcList 0 ("Struct.StructConstr" :: state)) do
                    yield Struct.StructConstr(v, pl)
        }

and GenerateEntity (state:string list) : Entity seq =
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            //EChar.    Entity   ::= Char ;
            yield Entity.EChar('c')
        
            //EStruct.  Entity   ::= Struct ;
            for s in (GenerateStruct ("Entity.EStruct" :: state)) do
                yield Entity.EStruct(s)

            //ECollect. Entity   ::= Collect ;
            for c in (GenerateCollect ("Entity.ECollect" :: state)) do
                yield Entity.ECollect(c)
        }

and GenerateQuantity (state:string list) : Quantity seq =
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            //QInt.     Quantity ::= Integer ;
            yield Quantity.QInt(101)

            //QDouble.  Quantity ::= Double ;
            yield Quantity.QDouble(101.2)
        }

and GenerateValue (state:string list) : Value seq =
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            //VQuant.   Value    ::= Quantity ;
            for q in (GenerateQuantity ("Value.VQuant" :: state)) do
                yield Value.VQuant(q)

            //VEnt.     Value    ::= Entity ;
            for ent in (GenerateEntity ("Value.VEnt" :: state)) do
                yield Value.VEnt(ent)
        }

and GenerateCBranch (state:string list) : CBranch seq =
    //-- Choice branch
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            //Choice. CBranch ::= "case" [Bind] "=>" "{" Proc "}" ;
            for bl in (GenerateBindList ("CBranch.Choice" :: state)) do
                for p in (GenerateProc 0 ("CBranch.Choice" :: state)) do
                    yield CBranch.Choice(bl, p)
        }

and GenerateCBranchList (state:string list) : CBranch list seq =
    //separator nonempty CBranch "" ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<CBranch> state GenerateCBranch true

and GeneratePMBranch (state:string list) : PMBranch seq =
    //-- Pattern match branches
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            //PatternMatch. PMBranch ::= PPattern "=>" "{" Proc "}" ;
            for pp in (GeneratePPattern 0 ("PMBranch.PatternMatch" :: state)) do
                for p in (GenerateProc 0 ("PMBranch.PatternMatch" :: state)) do
                    yield PMBranch.PatternMatch(pp, p)
        }
    
and GeneratePMBranchList (state:string list) : PMBranch list seq =
    //separator nonempty PMBranch "" ; 
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<PMBranch> state GeneratePMBranch true

and GenerateBind (state:string list) : Bind seq =
    //-- Variable binding
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            //InputBind. Bind ::= CPattern "<-" Chan ;
            for cp in (GenerateCPattern ("Bind.InputBind" :: state)) do
                for c in (GenerateChan ("Bind.InputBind" :: state)) do
                    yield Bind.InputBind(cp, c)
        }

and GenerateBindList (state:string list) : Bind list seq =
    //separator nonempty Bind ";" ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<Bind> state GenerateBind true

and GenerateChan (state:string list) : Chan seq =
    //-- Channels
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            //CVar.    Chan ::= Var ;
            for v in (GenerateVar ("Chan.CVar" :: state)) do
                yield Chan.CVar(v)

            //CQuote.  Chan ::= "@" Proc3 ;
            for p3 in (GenerateProc 3 ("" :: state)) do
                yield Chan.CQuote(p3)
        }

and GenerateProc (n:int) (state:string list) : Proc seq =
    //-- Processes
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        seq {
            if (n = 0 || n = 4) then
                //PNil.    Proc4 ::= "Nil" ;
                yield Proc.PNil

                //PValue.  Proc4 ::= Value ;
                for v in (GenerateValue ("Proc.PValue" :: state)) do
                    yield Proc.PValue(v)

                //PVar.    Proc4 ::= Var ;
                for v in (GenerateVar ("Proc.PVar" :: state)) do
                    yield Proc.PVar(v)

            if (n = 0 || n = 3) then
                //PDrop.   Proc3 ::= "*" Chan ;
                for c in (GenerateChan ("Proc.PDrop" :: state)) do
                    yield Proc.PDrop(c)

                //PInject. Proc3 ::= "#" Chan ;
                for c in (GenerateChan ("Proc.PInject" :: state)) do
                    yield Proc.PInject(c)

            if (n = 0 || n = 2) then
                //PLift.   Proc2 ::= Chan "!" "(" [Proc] ")" ;
                for c in (GenerateChan ("Proc.PLift" :: state)) do
                    for pl in (GenerateProcList 0 ("Proc.PLift" :: state)) do
                        yield Proc.PLift(c, pl)

            if (n = 0 || n = 1) then
                //PInput.  Proc1 ::= "for" "(" [Bind] ")" "{" Proc "}" ;
                for bl in (GenerateBindList ("Proc.PInput" :: state)) do
                    for p in (GenerateProc 0 ("Proc.PInput" :: state)) do
                        yield Proc.PInput(bl, p)

                //PChoice. Proc1 ::= "select" "{" [CBranch] "}" ;
                for cbl in (GenerateCBranchList ("Proc.PChoice" :: state)) do
                    yield Proc.PChoice(cbl)

                //PMatch.  Proc1 ::= "match" Proc "with" [PMBranch] 
                for p in (GenerateProc 0 ("Proc.PMatch" :: state)) do
                    for pmbl in (GeneratePMBranchList ("Proc.PMatch" :: state)) do
                        yield Proc.PMatch(p, pmbl)

                //PNew.    Proc1 ::= "new" [Var] "in" Proc1 ;
                for vl in (GenerateVarList ("Proc.PNew" :: state)) do
                    for p1 in (GenerateProc 1 ("Proc.PNew" :: state)) do
                        yield Proc.PNew(vl, p1)

                //PConstr. Proc1 ::= Name "(" [Proc] ")" ;
                for name in (GenerateName ("Proc.PConstr" :: state)) do
                    for pl in (GenerateProcList 0 ("Proc.PConstr" :: state)) do
                        yield Proc.PConstr(name, pl)

            //PPar.    Proc  ::= Proc "|" Proc1 ;
            for pl in (GenerateProcList 0 ("" :: state)) do
                yield Proc.PPar(pl)
        }

and GenerateProcList (n:int) (state:string list) : Proc list seq =
    //separator nonempty Proc "," ;
    match state with
    | h :: t when List.contains h t ->
        // Tail contains the head, cycle detected
        Seq.empty
    | _ ->
        GenerateList<Proc> state (GenerateProc 0) true

let GenerateContr () : Contr seq =
    // DContr. Contr ::= "contract" Name "(" [CPattern] ")" "=" "{" Proc "}" ;
    seq {
        for name in (GenerateName [ "Contr.DContr"; ]) do
            for cpl in (GenerateCPatternList [ "Contr.DContr"; ]) do
                let contract = Contr.DContr(name, cpl, Proc.PNil)
                printfn "%A" contract
                yield contract
    }
