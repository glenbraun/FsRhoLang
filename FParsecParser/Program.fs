#nowarn "40"

open System

open FParsec
open FParsec.CharParsers

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

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
    | PPtOutput of CPattern * PPattern option
    | PPtInput of PatternBind option * PPattern
    | PPtMatch of PPattern * PatternMatchBranchPattern option
    | PPtNew of VarPattern option * PPattern
    | PPtConstr of Name * PPattern option
    | PPtPar of PPattern * PPattern

and ValPattern =
    | VPtStruct of Var * PPattern option

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
    | StructConstr of Var * Proc

and Proc =
    | PNil
    | PValue of Value
    | PVar of Var
    | PDrop of Chan
    | PInject of Chan
    | PLift of Chan * Proc option
    | PInput of Bind option * Proc
    | PChoice of CBranch option
    | PMatch of Proc * PMBranch option
    | PNew of Var option * Proc
    | PConstr of Name * Proc option
    | PPar of Proc * Proc

and Collect =
    | CString of string

and Bind =
    | InputBind of CPattern * Chan

and Chan =
    | CVar of Var
    | CQuote of Proc

and CBranch =
    | Choice of Bind option * Proc

and PMBranch =
    | PatternMatch of PPattern * Proc

and Contr =
    | DContr of Name * CPattern list option * Proc list

[<EntryPoint>]
let main argv = 
    // Forward references
    let pPPattern, pPPatternRef = createParserForwardedToRef<PPattern, unit>()
    let pPPattern1, pPPattern1Ref = createParserForwardedToRef<PPattern, unit>()
    let pPPattern3, pPPattern3Ref = createParserForwardedToRef<PPattern, unit>()
    let pCPattern, pCPatternRef = createParserForwardedToRef<CPattern, unit>()
    let pVarPattern, pVarPatternRef = createParserForwardedToRef<VarPattern, unit>()
    let pProc, pProcRef = createParserForwardedToRef<Proc, unit>()
    let pProc1, pProc1Ref = createParserForwardedToRef<Proc, unit>()
    let pProc3, pProc3Ref = createParserForwardedToRef<Proc, unit>()
    let pBind, pBindRef = createParserForwardedToRef<Bind, unit>()
    let pChan, pChanRef = createParserForwardedToRef<Chan, unit>()


    let ws =
        unicodeSpaces

    let ws1 = 
        unicodeSpaces1

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

        between (pstring "\"") (pstring "\"")
                (stringsSepBy normalCharSnippet escapedCharSnippet)

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

    let pIntegerLiteral =
        pint32

    let pDoubleLiteral = 
        pfloat

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


    let pVar = 
        // token Var (lower (letter | digit | '_' | '\'')*) ;
        let isVarFirstChar c = isLower c
        let isVarChar c = isLetter c || isDigit c || c = '_' || c = '\''
        
        notFollowedBy (pKeyWords) >>.
        (many1Satisfy2L isVarFirstChar isVarChar "Var" .>> ws) |>>
        (fun x -> Var.VarValue(x))

    let pName = 
        // token Name (upper (letter | digit | '_' | '\'')*) ;
        let isNameFirstChar c = isUpper c
        let isNameChar c = isLetter c || isDigit c || c = '_' || c = '\''

        notFollowedBy (skipString "Nil") >>.
        (many1Satisfy2L isNameFirstChar isNameChar "Name" .>> ws) |>>
        (fun x -> Name.NameValue(x))

    //-- Value patterns
    let pVPtStruct =
        //VPtStruct. ValPattern ::= Var "{" [PPattern] "}" ;
        pipe4 
            (pVar)
            (skipString "{" .>> ws)
            (opt pPPattern)
            (skipString "}" .>> ws)
            (fun a b c d -> ValPattern.VPtStruct(a, c))

    let pValPattern =
       pVPtStruct

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

    let pPatternPatternMatch =
        pPtBranch

    let pPatternPatternMatchList = 
        //separator nonempty PatternPatternMatch "" ;
        sepBy (pPatternPatternMatch) ws

    //-- Bind pattern
    let pPtBind =
        //PtBind.   PatternBind ::= CPattern "<-" CPattern ;
        pipe3 
            (pCPattern .>> ws)
            (skipString "<-" .>> ws)
            (pCPattern .>> ws)
            (fun a b c -> PatternBind.PtBind(a, c))

    let pPatternBind = 
        pPtBind

    let pPatternBindList = 
        //separator nonempty PatternBind ";" ;
        sepBy1 pPatternBind (skipString ";" .>> ws)   

    // -- Channel patterns
    let pCPtVar = 
        //CPtVar.    CPattern ::= VarPattern ;
        pVarPattern |>>
        (fun x -> CPattern.CPtVar(x))

    let pCPtQuote =
        //CPtQuote.  CPattern ::= "@" PPattern3 ;
        pipe2
            (skipString "@")
            (pPPattern3)
            (fun a b -> CPattern.CPtQuote(b))

    let pCPatternList =
        //separator CPattern "," ;
        sepBy1 (pCPattern) (skipString "," .>> ws )

    //-- Process patterns
    let pPPtVar =
        //PPtVar.    PPattern4 ::= VarPattern ;
        pVarPattern |>>
        (fun x -> PPattern.PPtVar(x))

    let pPPtNil =
        //PPtNil.    PPattern4 ::= "Nil" ;
        (skipString "Nil" .>> ws) |>>
        (fun () -> PPattern.PPtNil)

    let pPPtVal =
        //PPtVal.    PPattern4 ::= ValPattern ;
        pValPattern |>>
        (fun x -> PPattern.PPtVal(x))

    let pPPattern4 =
        pPPtVal <|> pPPtNil <|> pPPtVar

    let pPPtDrop =
        //PPtDrop.   PPattern3 ::= "*" CPattern ;
        (pstring "*") >>. pCPattern |>>
        (fun x -> PPattern.PPtDrop(x))
         
    let pPPtInject =
        //PPtInject. PPattern3 ::= "#" CPattern ;
        (pstring "#") >>. pCPattern |>>
        (fun x -> PPattern.PPtInject(x))

    let pPPtOutput =
        //PPtOutput. PPattern2 ::= CPattern "!" "(" [PPattern] ")" ;
        pipe3 
            pCPattern
            (skipString "!" .>> ws) 
            (between 
                (skipString "(")
                (skipString ")")
                ((opt pPPattern) .>> ws)
            )
            (fun a b c -> PPattern.PPtOutput(a, c))

    let pPPattern2 = pPPtOutput

    let pPPtInput =
        //PPtInput.  PPattern1 ::= "for" "(" [PatternBind] ")" "{" PPattern "}" ;
        pipe3
            (skipString "for" .>> ws)
            (between (skipString "(") (skipString ")") ((opt pPatternBind) .>> ws))
            (between (skipString "{") (skipString "}") (pPPattern .>> ws))
            (fun a b c -> PPattern.PPtInput(b, c))

    let pPPtMatch =
        //PPtMatch.  PPattern1 ::= "match" PPattern "with" [PatternPatternMatch] ;        
        pipe4 
            (skipString "match" .>> ws)
            (pPPattern .>> ws)
            (skipString "with" .>> ws)
            ((opt pPatternPatternMatch) .>> ws)
            (fun a b c d -> PPattern.PPtMatch(b, d))

    let pPPtNew =
        //PPtNew.    PPattern1 ::= "new" [VarPattern] "in" PPattern1 ;
        pipe4
            (skipString "new" .>> ws)
            (opt pVarPattern .>> ws)
            (skipString "in" .>> ws)
            (pPPattern1)
            (fun a b c d -> PPattern.PPtNew(b, d))

    let pPPtConstr =
        //PPtConstr. PPattern1 ::= Name "(" [PPattern] ")" ;
        pipe2
            pName
            (between 
                (pstring "(") 
                (pstring ")") 
                ((opt pPPattern) .>> ws)
            )
            (fun a b -> PPattern.PPtConstr(a, b))

    let pPPtPar =
        //PPtPar.    PPattern  ::= PPattern "|" PPattern1 ;
        pipe3
            (pPPattern)
            ((skipString "|") .>> ws)
            (pPPattern1)
            (fun a b c -> PPattern.PPtPar(a, c))

    let pPPatternList =
        //separator PPattern "," ;
        sepBy1 (pPPattern) (skipString "," .>> ws)
    //coercions PPattern 4 ;

    // -- Variable patterns
    let pVarPtVar =
        // VarPtVar.  VarPattern ::= Var ;
        pVar |>>
        (fun x -> VarPattern.VarPtVar(x))

    let pVarPtWild =
        // VarPtWild. VarPattern ::= "_" ;
        skipString "_" .>> ws |>>
        (fun () -> VarPattern.VarPtWild)

    let pVarPatternList() =
        // separator VarPattern "," ;
        sepBy (pVarPattern) (pchar ',' .>> ws)

    //-- Values
    let pCString =
        //CString. Collect ::= String ;
        pStringLiteral |>>
        (fun x -> Collect.CString(x))
    //-- CArray.  Collect ::= Array ;
    //-- CList.   Collect ::= List ;

    let pCollect = 
        pCString  // eventually CArray and CList too

    let pStructConstr =
        //StructConstr. Struct ::= Var "{" [Proc] "}" ;
        pipe2
            pVar
            (between 
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProc .>> ws)
            )
            (fun a b -> Struct.StructConstr(a, b))

    let pStruct =
        pStructConstr

    let pEChar =
        //EChar.    Entity   ::= Char ;
        pCharLiteral |>>
        (fun x -> Entity.EChar(x)) 

    //-- EDate.    Entity   ::= Datetime ;
    
    let pEStruct =
        //EStruct.  Entity   ::= Struct ;
        pStruct |>>
        (fun x -> Entity.EStruct(x))
    
    let pECollect =
        //ECollect. Entity   ::= Collect ;
        pCollect |>>
        (fun x -> Entity.ECollect(x))

    let pEntity =
        pECollect <|> pEStruct <|> pEChar

    //-- QBool.    Quantity ::= Boolean ;
    
    let pQInt =
        //QInt.     Quantity ::= Integer ;
        pIntegerLiteral |>>
        (fun x -> Quantity.QInt(x))

    let pQDouble =
        //QDouble.  Quantity ::= Double ;
        pDoubleLiteral |>>
        (fun x -> Quantity.QDouble(x))

    let pQuantity =
        pQDouble <|> pQInt

    let pVQuant =
           //VQuant.   Value    ::= Quantity ;
           pQuantity |>>
           (fun x -> Value.VQuant(x))

    let pVEnt =
        //VEnt.     Value    ::= Entity ;
        pEntity |>>
        (fun x -> Value.VEnt(x))

    let pValue =
        pVEnt <|> pVQuant

    //-- Choice branch
    let pChoice =
        //Choice. CBranch ::= "case" [Bind] "=>" "{" Proc "}" ;
        pipe4
            (skipString "case" .>> ws)
            (opt pBind)
            (skipString "=>" .>> ws)
            (between
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProc .>> ws)
            )
            (fun a b c d -> CBranch.Choice(b, d))

    let pCBranch = 
        pChoice

    let pChoiceList =
        //separator nonempty CBranch "" ;
        many pCBranch

    //-- Pattern match branches
    let pPatternMatch =
        //PatternMatch. PMBranch ::= PPattern "=>" "{" Proc "}" ;
        pipe3
            pPPattern
            (skipString "=>" .>> ws)
            (between
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProc .>> ws)
            )
            (fun a b c -> PMBranch.PatternMatch(a, c))

    let pPMBranch =
        pPatternMatch

    let pPMBranchList =
        //separator nonempty PMBranch "" ; 
        many pPMBranch

    //-- Variable binding
    let pInputBind =
        //InputBind. Bind ::= CPattern "<-" Chan ;
        pipe3
            pCPattern
            (skipString "<-" .>> ws)
            pChan
            (fun a b c -> Bind.InputBind(a, c))

    let pBindList =
        //separator nonempty Bind ";" ;
        sepBy1 pBind (skipString ";" .>> ws)

    //-- Channels
    let pCVar =
        //CVar.    Chan ::= Var ;
        pVar |>>
        (fun x -> Chan.CVar(x))

    let pCQuote =
        //CQuote.  Chan ::= "@" Proc3 ;
        (skipString "@") >>.
        pProc3 |>>
        (fun x -> Chan.CQuote(x))

    //-- Processes
    let pPNil =
        //PNil.    Proc4 ::= "Nil" ;
        (skipString "Nil" .>> ws) |>>
        (fun () -> Proc.PNil)

    let pPValue =
        //PValue.  Proc4 ::= Value ;
        pValue |>>
        (fun x -> Proc.PValue(x))

    let pPVar =
        //PVar.    Proc4 ::= Var ;
        pVar |>>
        (fun x -> Proc.PVar(x))

    let pProc4 =
        pPVar <|> pPValue <|> pPNil

    let pPDrop = 
        //PDrop.   Proc3 ::= "*" Chan ;
        (skipString "*") >>.
        pChan |>>
        (fun x -> Proc.PDrop(x))

    let pPInject =
        //PInject. Proc3 ::= "#" Chan ;
        (skipString "#") >>.
        pChan |>>
        (fun x -> Proc.PInject(x))

    let pPLift =
        //PLift.   Proc2 ::= Chan "!" "(" [Proc] ")" ;
        pipe3
            pChan
            (skipString "!" .>> ws)
            (between
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                ((opt pProc) .>> ws)
            )
            (fun a b c -> Proc.PLift(a, c))

    let pProc2 = 
        pPLift

    let pPInput =
        //PInput.  Proc1 ::= "for" "(" [Bind] ")" "{" Proc "}" ;
        pipe3 
            (skipString "for" .>> ws)
            (between
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                ((opt pBind) .>> ws)
            )
            (between
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProc .>> ws)
            )
            (fun a b c -> Proc.PInput(b, c))

    let pPChoice =
        //PChoice. Proc1 ::= "select" "{" [CBranch] "}" ;
        (skipString "select" .>> ws) >>.
        (between
            (skipString "{" .>> ws)
            (skipString "}" .>> ws)
            ((opt pCBranch) .>> ws)
        ) |>>
        (fun x -> Proc.PChoice(x))

    let pPMatch =
        //PMatch.  Proc1 ::= "match" Proc "with" [PMBranch] ;
        pipe4
            (skipString "match" .>> ws)
            (pProc .>> ws)
            (skipString "with" .>> ws)
            (opt pPMBranch)
            (fun a b c d -> Proc.PMatch(b, d))

    let pPNew =
        //PNew.    Proc1 ::= "new" [Var] "in" Proc1 ;
        pipe4
            (skipString "new" .>> ws)
            (opt pVar)
            (skipString "in" .>> ws)
            pProc1
            (fun a b c d -> Proc.PNew(b, d))

    let pPConstr =
        //PConstr. Proc1 ::= Name "(" [Proc] ")" ;
        pipe2
            pName
            (between
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                ((opt pProc) .>> ws)
            )
            (fun a b -> Proc.PConstr(a, b))

    let pPPar =
        //PPar.    Proc  ::= Proc "|" Proc1 ;

        pipe3
            pProc
            (skipString "|" .>> ws)
            pProc1
            (fun a b c -> Proc.PPar(a, c))

    let pProcList = 
        //separator nonempty Proc "," ;
        sepBy1 pProc (skipString "," .>> ws)
        //coercions Proc 4 ;

    //-- Top level contract declaration
    let pDContr =
        //DContr. Contr ::= "contract" Name "(" [CPattern] ")" "=" "{" Proc "}" ;
        ws >>.
        (pipe5
            (skipString "contract" .>> ws)
            pName
            (between
                (skipString "(" .>> ws)
                (skipString ")" .>> ws)
                ((opt pCPatternList) .>> ws)
            )
            (skipString "=" .>> ws)
            (between
                (skipString "{" .>> ws)
                (skipString "}" .>> ws)
                (pProcList .>> ws)
            )
            (fun a b c d e -> Contr.DContr(b, c, e))
        )

    let pContr =
        pDContr

    
    pPPatternRef :=
        choice [
            pPPattern4
            pPPattern3
            pPPattern2
            pPPattern1
        ]

    pPPattern1Ref :=
        choice [
            pPPtInput
            pPPtMatch 
            pPPtNew 
            pPPtConstr 
        ]

    pPPattern3Ref :=
        choice [
            pPPtDrop 
            pPPtInject
        ]

    pCPatternRef :=
        choice [
            pCPtVar
            pCPtQuote
        ]

    pVarPatternRef := 
        choice [
            pVarPtVar
            pVarPtWild
        ]

    pProcRef :=
        choice [
            pProc4
            pProc3
            pProc2
            pProc1
        ]

    pProc1Ref :=
        choice [
            pPInput
            pPChoice
            pPMatch
            pPNew
            pPConstr
        ]

    pProc3Ref :=
        choice [
            pPDrop
            pPInject
        ]

    pBindRef :=
        pInputBind

    pChanRef :=
        choice [
            pCVar
            pCQuote
        ]

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

    0 // return an integer exit code
