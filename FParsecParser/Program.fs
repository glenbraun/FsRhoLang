#nowarn "40"

open System

open FParsec
open FParsec.CharParsers

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

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

[<EntryPoint>]
let main argv = 
    // Forward references
    let pPPattern, pPPatternRef = createParserForwardedToRef<PPattern, unit>()
    let pPPattern1, pPPattern1Ref = createParserForwardedToRef<PPattern, unit>()
    let pPPattern3, pPPattern3Ref = createParserForwardedToRef<PPattern, unit>()
    let pPPatternList, pPPatternListRef = createParserForwardedToRef<PPattern list, unit>()
    let pCPattern, pCPatternRef = createParserForwardedToRef<CPattern, unit>()
    let pVarPattern, pVarPatternRef = createParserForwardedToRef<VarPattern, unit>()
    let pVarPatternList, pVarPatternListRef = createParserForwardedToRef<VarPattern list, unit>()
    let pProc, pProcRef = createParserForwardedToRef<Proc, unit>()
    let pProc1, pProc1Ref = createParserForwardedToRef<Proc, unit>()
    let pProc3, pProc3Ref = createParserForwardedToRef<Proc, unit>()
    let pProcList, pProcListRef = createParserForwardedToRef<Proc list, unit>()
    let pBind, pBindRef = createParserForwardedToRef<Bind, unit>()
    let pBindList, pBindListRef = createParserForwardedToRef<Bind list, unit>()
    let pChan, pChanRef = createParserForwardedToRef<Chan, unit>()


    let ws =
        unicodeSpaces
        <!> "ws"

    let ws1 = 
        unicodeSpaces1
        <!> "ws1"

    // -- Literals
    let pStringLiteral =
        let escape =  anyOf "\"\\/bfnrt"
                      |>> function
                          | 'b' -> "\b"
                          | 'f' -> "\u000C"
                          | 'n' -> "\n"
                          | 'r' -> "\r"
                          | 't' -> "\t"
                          | c   -> string c // every other char is mapped to itself

        let unicodeEscape =
            // converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

        (between (pstring "\"") (pstring "\"")
                (stringsSepBy normalCharSnippet escapedCharSnippet)
        )
        <!> "pStringLiteral"

    let pCharLiteral =
        let escape =  anyOf "\"\\/bfnrt"
                      |>> function
                          | 'b' -> '\b'
                          | 'f' -> '\u000C'
                          | 'n' -> '\n'
                          | 'r' -> '\r'
                          | 't' -> '\t'
                          | c   -> c // every other char is mapped to itself

        let unicodeEscape =
            // converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char
            )

        let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet  = satisfy (fun c -> c <> '"' && c <> '\\')
        (between
            (skipString "\'")
            (skipString "\'")
            (escapedCharSnippet <|> normalCharSnippet)
        )
        <!> "pCharLiteral"

    let pIntegerLiteral =
        pint32
        <!> "pIntegerLiteral"

    let pDoubleLiteral = 
        pfloat
        <!> "pDoubleLiteral"

    // -- Names and variables
    let pKeyWords =
        (skipString "Nil" .>> ws) <|>
        (skipString "contract" .>> ws) <|>
        (skipString "for" .>> ws) <|>
        (skipString "select" .>> ws) <|>
        (skipString "match" .>> ws) <|>
        (skipString "with" .>> ws) <|>
        (skipString "new" .>> ws) <|>
        (skipString "case" .>> ws) <|>
        (skipString "true" .>> ws) <|>
        (skipString "false" .>> ws) <|>
        (skipString "in" .>> ws)
        <!> "pKeyWords"

    let pVar = 
        // token Var (lower (letter | digit | '_' | '\'')*) ;
        let isVarFirstChar c = isLower c
        let isVarChar c = isLetter c || isDigit c || c = '_' || c = '\''
        
        notFollowedBy (pKeyWords) >>.
        (many1Satisfy2L isVarFirstChar isVarChar "Var" .>> ws) |>>
        (fun x -> Var.VarValue(x))
        <!> "pVar"

    let pVarList = 
        // separator nonempty Var "," ;
        sepBy1 (pVar .>> ws) (skipString "," .>> ws)
        <!> "pVarList"

    let pName = 
        // token Name (upper (letter | digit | '_' | '\'')*) ;
        let isNameFirstChar c = isUpper c
        let isNameChar c = isLetter c || isDigit c || c = '_' || c = '\''

        notFollowedBy (skipString "Nil") >>.
        (many1Satisfy2L isNameFirstChar isNameChar "Name" .>> ws) |>>
        (fun x -> Name.NameValue(x))
        <!> "pName"

    let pNameList =
        // separator nonempty Name "," ;
        sepBy1 (pName .>> ws) (skipString "," .>> ws)
        <!> "pNameList"

    //-- Value patterns
    let pVPtStruct =
        //VPtStruct. ValPattern ::= Var "{" [PPattern] "}" ;
        pipe4 
            (pVar)
            (skipString "{" .>> ws)
            (pPPatternList .>> ws)
            (skipString "}" .>> ws)
            (fun a b c d -> ValPattern.VPtStruct(a, c))
        <!> "pVPtStruct"

    let pValPattern =
       pVPtStruct
       <!> "pValPattern"

    //-- Pattern match branch pattern
    let pPtBranch =
        //PtBranch. PatternPatternMatch ::= PPattern "=>" "{" PPattern "}" ;
        pipe3
            (pPPattern .>> ws)
            (skipString "=>" .>> ws)
            (between 
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pPPattern .>> ws)
            )
            (fun a b c -> PatternMatchBranchPattern.PtBranch(a, c))
        <!> "pPtBranch"

    let pPatternPatternMatch =
        pPtBranch
        <!> "pPatternPatternMatch"

    let pPatternPatternMatchList = 
        //separator nonempty PatternPatternMatch "" ;
        many (pPatternPatternMatch .>> ws)
        <!> "pPatternPatternMatchList"

    //-- Bind pattern
    let pPtBind =
        //PtBind.   PatternBind ::= CPattern "<-" CPattern ;
        pipe3 
            (pCPattern .>> ws)
            (skipString "<-" .>> ws)
            (pCPattern .>> ws)
            (fun a b c -> PatternBind.PtBind(a, c))
        <!> "pPtBind"

    let pPatternBind = 
        pPtBind
        <!> "pPatternBind"

    let pPatternBindList = 
        //separator nonempty PatternBind ";" ;
        sepBy1 (pPatternBind .>> ws) (skipString ";" .>> ws)   
        <!> "pPatternBindList"

    // -- Channel patterns
    let pCPtVar = 
        //CPtVar.    CPattern ::= VarPattern ;
        (pVarPattern .>> ws) |>>
        (fun x -> CPattern.CPtVar(x))
        <!> "pCPtVar"

    let pCPtQuote =
        //CPtQuote.  CPattern ::= "@" PPattern3 ;
        pipe2
            (skipString "@")
            (pPPattern3 .>> ws)
            (fun a b -> CPattern.CPtQuote(b))
        <!> "pCPtQuote"

    let pCPatternList =
        //separator CPattern "," ;
        sepBy1 (pCPattern .>> ws) (skipString "," .>> ws)
        <!> "pCPatternList"

    //-- Process patterns
    let pPPtVar =
        //PPtVar.    PPattern4 ::= VarPattern ;
        (pVarPattern .>> ws) |>>
        (fun x -> PPattern.PPtVar(x))
        <!> "pPPtVar"

    let pPPtNil =
        //PPtNil.    PPattern4 ::= "Nil" ;
        (skipString "Nil" .>> ws) |>>
        (fun () -> PPattern.PPtNil)
        <!> "pPPtNil"

    let pPPtVal =
        //PPtVal.    PPattern4 ::= ValPattern ;
        (pValPattern .>> ws) |>>
        (fun x -> PPattern.PPtVal(x))
        <!> "pPPtVal"

    let pPPattern4 =
        pPPtVal <|> pPPtNil <|> pPPtVar
        <!> "pPPattern4"

    let pPPtDrop =
        //PPtDrop.   PPattern3 ::= "*" CPattern ;
        (skipString "*") >>. (pCPattern .>> ws) |>>
        (fun x -> PPattern.PPtDrop(x))
        <!> "pPPtDrop"

    let pPPtInject =
        //PPtInject. PPattern3 ::= "#" CPattern ;
        (skipString "#") >>. (pCPattern .>> ws) |>>
        (fun x -> PPattern.PPtInject(x))
        <!> "pPPtInject"

    let pPPtOutput =
        //PPtOutput. PPattern2 ::= CPattern "!" "(" [PPattern] ")" ;
        pipe3 
            pCPattern
            (skipString "!" .>> ws) 
            (between 
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                (pPPatternList .>> ws)
            )
            (fun a b c -> PPattern.PPtOutput(a, c))
        <!> "pPPtOutput"

    let pPPattern2 = 
        pPPtOutput
        <!> "pPPattern2"

    let pPPtInput =
        //PPtInput.  PPattern1 ::= "for" "(" [PatternBind] ")" "{" PPattern "}" ;
        pipe3
            (skipString "for" .>> ws)
            (between 
                (skipString "(" .>> ws) 
                (skipString ")" .>> ws) 
                (pPatternBindList .>> ws)
            )
            (between 
                (skipString "{" .>> ws)
                (skipString "}" .>> ws) 
                (pPPattern .>> ws)
            )
            (fun a b c -> PPattern.PPtInput(b, c))
        <!> "pPPtInput"

    let pPPtMatch =
        //PPtMatch.  PPattern1 ::= "match" PPattern "with" [PatternPatternMatch] ;        
        pipe4 
            (skipString "match" .>> ws)
            (pPPattern .>> ws)
            (skipString "with" .>> ws)
            (pPatternPatternMatchList .>> ws)
            (fun a b c d -> PPattern.PPtMatch(b, d))
        <!> "pPPtMatch"

    let pPPtNew =
        //PPtNew.    PPattern1 ::= "new" [VarPattern] "in" PPattern1 ;
        pipe4
            (skipString "new" .>> ws)
            (pVarPatternList .>> ws)
            (skipString "in" .>> ws)
            (pPPattern1 .>> ws)
            (fun a b c d -> PPattern.PPtNew(b, d))
        <!> "pPPtNew"

    let pPPtConstr =
        //PPtConstr. PPattern1 ::= Name "(" [PPattern] ")" ;
        pipe2
            pName
            (between 
                (skipString "(" .>> ws) 
                (skipString ")" .>> ws) 
                (pPPatternList .>> ws)
            )
            (fun a b -> PPattern.PPtConstr(a, b))
        <!> "pPPtConstr"

    let pPPtPar =
        //PPtPar.    PPattern  ::= PPattern "|" PPattern1 ;
        sepBy1 (pPPattern .>> ws) (skipString "|" .>> ws) |>>
        (fun x -> PPattern.PPtPar(x))
        <!> "pPPtPar"

    // -- Variable patterns
    let pVarPtVar =
        // VarPtVar.  VarPattern ::= Var ;
        (pVar .>> ws) |>>
        (fun x -> VarPattern.VarPtVar(x))
        <!> "pVarPtVar"

    let pVarPtWild =
        // VarPtWild. VarPattern ::= "_" ;
        skipString "_" .>> ws |>>
        (fun () -> VarPattern.VarPtWild)
        <!> "pVarPtWild"

    //-- Values
    //-- CArray.  Collect ::= Array ;
    //-- CList.   Collect ::= List ;

    let pCString =
        //CString. Collect ::= String ;
        (pStringLiteral .>> ws) |>>
        (fun x -> Collect.CString(x))
        <!> "pCString"

    let pCollect = 
        pCString  // eventually CArray and CList too
        <!> "pCollect"

    let pStructConstr =
        //StructConstr. Struct ::= Var "{" [Proc] "}" ;
        pipe2
            pVar
            (between 
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProcList .>> ws)
            )
            (fun a b -> Struct.StructConstr(a, b))
        <!> "pStructConstr"

    let pStruct =
        pStructConstr
        <!> "pStruct"

    let pEChar =
        //EChar.    Entity   ::= Char ;
        (pCharLiteral .>> ws) |>>
        (fun x -> Entity.EChar(x)) 
        <!> "pEChar"

    //-- EDate.    Entity   ::= Datetime ;
    
    let pEStruct =
        //EStruct.  Entity   ::= Struct ;
        (pStruct .>> ws) |>>
        (fun x -> Entity.EStruct(x))
        <!> "pEStruct"

    let pECollect =
        //ECollect. Entity   ::= Collect ;
        (pCollect .>> ws) |>>
        (fun x -> Entity.ECollect(x))
        <!> "pECollect"

    let pEntity =
        pECollect <|> pEStruct <|> pEChar
        <!> "pEntity"

    //-- QBool.    Quantity ::= Boolean ;
    
    let pQInt =
        //QInt.     Quantity ::= Integer ;
        (pIntegerLiteral .>> ws) |>>
        (fun x -> Quantity.QInt(x))
        <!> "pQInt"

    let pQDouble =
        //QDouble.  Quantity ::= Double ;
        (pDoubleLiteral .>> ws) |>>
        (fun x -> Quantity.QDouble(x))
        <!> "pQDouble"

    let pQuantity =
        pQDouble <|> pQInt
        <!> "pQuantity"

    let pVQuant =
           //VQuant.   Value    ::= Quantity ;
           (pQuantity .>> ws) |>>
           (fun x -> Value.VQuant(x))
           <!> "pVQuant"

    let pVEnt =
        //VEnt.     Value    ::= Entity ;
        (pEntity .>> ws) |>>
        (fun x -> Value.VEnt(x))
        <!> "pVEnt"

    let pValue =
        pVEnt <|> pVQuant
        <!> "pValue"

    //-- Choice branch
    let pChoice =
        //Choice. CBranch ::= "case" [Bind] "=>" "{" Proc "}" ;
        pipe4
            (skipString "case" .>> ws)
            (pBindList .>> ws)
            (skipString "=>" .>> ws)
            (between
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProc .>> ws)
            )
            (fun a b c d -> CBranch.Choice(b, d))
        <!> "pChoice"

    let pCBranch = 
        pChoice
        <!> "pCBranch"

    let pCBranchList =
        //separator nonempty CBranch "" ;
        many pCBranch
        <!> "pCBranchList"

    //-- Pattern match branches
    let pPatternMatch =
        //PatternMatch. PMBranch ::= PPattern "=>" "{" Proc "}" ;
        pipe3
            (pPPattern .>> ws)
            (skipString "=>" .>> ws)
            (between
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProc .>> ws)
            )
            (fun a b c -> PMBranch.PatternMatch(a, c))
        <!> "pPatternMatch"

    let pPMBranch =
        pPatternMatch
        <!> "pPMBranch"

    let pPMBranchList =
        //separator nonempty PMBranch "" ; 
        many pPMBranch
        <!> "pPMBranchList"

    //-- Variable binding
    let pInputBind =
        //InputBind. Bind ::= CPattern "<-" Chan ;
        pipe3
            (pCPattern .>> ws)
            (skipString "<-" .>> ws)
            (pChan .>> ws)
            (fun a b c -> Bind.InputBind(a, c))
        <!> "pInputBind"

    //-- Channels
    let pCVar =
        //CVar.    Chan ::= Var ;
        (pVar .>> ws) |>>
        (fun x -> Chan.CVar(x))
        <!> "pCVar"

    let pCQuote =
        //CQuote.  Chan ::= "@" Proc3 ;
        (skipString "@") >>.
        (pProc3 .>> ws) |>>
        (fun x -> Chan.CQuote(x))
        <!> "pCQuote"

    //-- Processes
    let pPNil =
        //PNil.    Proc4 ::= "Nil" ;
        (skipString "Nil" .>> ws) |>>
        (fun () -> Proc.PNil)
        <!> "pPNil"

    let pPValue =
        //PValue.  Proc4 ::= Value ;
        (pValue .>> ws) |>>
        (fun x -> Proc.PValue(x))
        <!> "pPValue"

    let pPVar =
        //PVar.    Proc4 ::= Var ;
        (pVar .>> ws) |>>
        (fun x -> Proc.PVar(x))
        <!> "pPVar"

    let pProc4 =
        pPVar <|> pPValue <|> pPNil
        <!> "pProc4"

    let pPDrop = 
        //PDrop.   Proc3 ::= "*" Chan ;
        (skipString "*") >>.
        (pChan .>> ws) |>>
        (fun x -> Proc.PDrop(x))
        <!> "pPDrop"

    let pPInject =
        //PInject. Proc3 ::= "#" Chan ;
        (skipString "#") >>.
        (pChan .>> ws) |>>
        (fun x -> Proc.PInject(x))
        <!> "pPInject"

    let pPLift =
        //PLift.   Proc2 ::= Chan "!" "(" [Proc] ")" ;
        pipe3
            pChan
            (skipString "!" .>> ws)
            (between
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                (pProcList .>> ws)
            )
            (fun a b c -> Proc.PLift(a, c))
        <!> "pPLift"

    let pProc2 = 
        pPLift
        <!> "pProc2"

    let pPInput =
        //PInput.  Proc1 ::= "for" "(" [Bind] ")" "{" Proc "}" ;
        pipe3 
            (skipString "for" .>> ws)
            (between
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                (pBindList .>> ws)
            )
            (between
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProc .>> ws)
            )
            (fun a b c -> Proc.PInput(b, c))
        <!> "pPInput"

    let pPChoice =
        //PChoice. Proc1 ::= "select" "{" [CBranch] "}" ;
        (skipString "select" .>> ws) >>.
        (between
            (skipString "{" .>> ws)
            (skipString "}" .>> ws)
            (pCBranchList .>> ws)
        ) |>>
        (fun x -> Proc.PChoice(x))
        <!> "pPChoice"

    let pPMatch =
        //PMatch.  Proc1 ::= "match" Proc "with" [PMBranch] ;
        pipe4
            (skipString "match" .>> ws)
            (pProc .>> ws)
            (skipString "with" .>> ws)
            (pPMBranchList .>> ws)
            (fun a b c d -> Proc.PMatch(b, d))
        <!> "pPMatch"

    let pPNew =
        //PNew.    Proc1 ::= "new" [Var] "in" Proc1 ;
        pipe4
            (skipString "new" .>> ws)
            (pVarList .>> ws)
            (skipString "in" .>> ws)
            (pProc1 .>> ws)
            (fun a b c d -> Proc.PNew(b, d))
        <!> "pPNew"

    let pPConstr =
        //PConstr. Proc1 ::= Name "(" [Proc] ")" ;
        pipe2
            pName
            (between
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                (pProcList .>> ws)
            )
            (fun a b -> Proc.PConstr(a, b))
        <!> "pPConstr"

    let pPPar =
        //PPar.    Proc  ::= Proc "|" Proc1 ;
        sepBy1 (pProc .>> ws) (skipString "|" .>> ws) |>>
        (fun x -> Proc.PPar(x))
        <!> "pPPar"

    //-- Top level contract declaration
    let pDContr =
        //DContr. Contr ::= "contract" Name "(" [CPattern] ")" "=" "{" Proc "}" ;
        (pipe5
            (skipString "contract" .>> ws)
            (pName .>> ws)
            (between
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                (pCPatternList .>> ws)
            )
            (skipString "=" .>> ws)
            (between
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProcList .>> ws)
            )
            (fun a b c d e -> Contr.DContr(b, c, e))
        )
        <!> "pDContr"

    let pContr =
        pDContr
        <!> "pContr"

    pPPatternRef :=
        choice [
            pPPattern4
            pPPattern3
            pPPattern2
            pPPattern1
        ]
        <!> "pPPattern"

    pPPattern1Ref :=
        choice [
            pPPtInput
            pPPtMatch 
            pPPtNew 
            pPPtConstr 
        ]
        <!> "pPPattern1"

    pPPattern3Ref :=
        choice [
            pPPtDrop 
            pPPtInject
        ]
        <!> "pPPattern3"

    pPPatternListRef :=
        // separator PPattern "," ;
        sepBy (pPPattern .>> ws) (skipString "," .>> ws)
        <!> "pPPatternList"

    pCPatternRef :=
        choice [
            pCPtVar
            pCPtQuote
        ]
        <!> "pCPattern"

    pVarPatternRef := 
        choice [
            pVarPtVar
            pVarPtWild
        ]
        <!> "pVarPattern"

    pVarPatternListRef :=
        // separator VarPattern "," ;
        sepBy (pVarPattern .>> ws) (skipString "," .>> ws)
        <!> "pVarPatternList"

    pProcRef :=
        choice [
            pProc4
            pProc3
            pProc2
            pProc1
        ]
        <!> "pProc"

    pProc1Ref :=
        choice [
            pPInput
            pPChoice
            pPMatch
            pPNew
            pPConstr
        ]
        <!> "pProc1"

    pProc3Ref :=
        choice [
            pPDrop
            pPInject
        ]
        <!> "pProc3"

    pProcListRef :=
        //separator nonempty Proc "," ;
        sepBy1 (pProc .>> ws) (skipString "," .>> ws)
        <!> "pProcList"

    pBindRef :=
        pInputBind
        <!> "pBind"

    pBindListRef :=
        // separator nonempty Bind ";" ;
        sepBy1 (pBind .>> ws) (skipString ";" .>> ws)
        <!> "pBindList"

    pChanRef :=
        choice [
            pCVar
            pCQuote
        ]
        <!> "pChan"

    test pVarPattern "a ,b            " 
    test pPatternBind "x <- e"
    test pContr "contract Glen () = { Nil }"
    test pContr "contract Glen (a) = { Nil }"
    test pContr "contract Glen (a,b) = { Nil }"
    test pContr "contract Glen (a, b) = { Nil }"

    test pProc "Nil"
    test pProc "1"
    test pCharLiteral "'a'"
    test pProc "'a'"
    test pProc4 "a"
    test pProc "a"

    test pContr "contract Glen (a, b) = { 'a'}"
    test pContr "contract Glen (a, b) = { 'a' }"

    test pContr "contract Glen (a, b) = { a }"
    test pContr "contract Glen (a, b) = { *a }"
    test pContr "contract Glen (a, b) = { #a }"

    test pContr "contract Glen (a, b) = { a!() }"
    test pContr "contract Glen (a, b) = { a!( Nil ) }"

    0 // return an integer exit code
