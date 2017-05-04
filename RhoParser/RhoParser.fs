module Rholang.Parser
#nowarn "40"

open System
open FParsec
open FParsec.CharParsers
open Rholang.AST

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        System.Diagnostics.Trace.WriteLine(sprintf "%A: Entering %s" stream.Position label)
        let reply = p stream
        System.Diagnostics.Trace.WriteLine(sprintf "%A: Leaving %s (%A)" stream.Position label reply.Status)
        reply

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
    <!> "StringLiteral"

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
    <!> "CharLiteral"

let pIntegerLiteral =
    pint32
    <!> "IntegerLiteral"

let pDoubleLiteral = 
    pfloat
    <!> "DoubleLiteral"

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
    <!> "KeyWords"

let pVar = 
    // token Var (lower (letter | digit | '_' | '\'')*) ;
    let isVarFirstChar c = isLower c
    let isVarChar c = isLetter c || isDigit c || c = '_' || c = '\''
        
    notFollowedBy (pKeyWords) >>.
    (many1Satisfy2L isVarFirstChar isVarChar "Var" .>> ws) |>>
    (fun x -> Var.VarValue(x))
    <!> "Var"

let pVarList = 
    // separator nonempty Var "," ;
    sepBy1 (pVar .>> ws) (skipString "," .>> ws)
    <!> "VarList"

let pName = 
    // token Name (upper (letter | digit | '_' | '\'')*) ;
    let isNameFirstChar c = isUpper c
    let isNameChar c = isLetter c || isDigit c || c = '_' || c = '\''

    notFollowedBy (skipString "Nil") >>.
    (many1Satisfy2L isNameFirstChar isNameChar "Name" .>> ws) |>>
    (fun x -> Name.NameValue(x))
    <!> "Name"

let pNameList =
    // separator nonempty Name "," ;
    sepBy1 (pName .>> ws) (skipString "," .>> ws)
    <!> "NameList"

//-- Value patterns
let pVPtStruct =
    //VPtStruct. ValPattern ::= Var "{" [PPattern] "}" ;
    pipe4 
        (pVar)
        (skipString "{" .>> ws)
        (pPPatternList .>> ws)
        (skipString "}" .>> ws)
        (fun a b c d -> ValPattern.VPtStruct(a, c))
    <!> "VPtStruct"

let pValPattern =
    pVPtStruct
    <!> "ValPattern"

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
        (fun a b c -> PatternPatternMatch.PtBranch(a, c))
    <!> "PtBranch"

let pPatternPatternMatch =
    pPtBranch
    <!> "PatternPatternMatch"

let pPatternPatternMatchList = 
    //separator nonempty PatternPatternMatch "" ;
    many (pPatternPatternMatch .>> ws)
    <!> "PatternPatternMatchList"

//-- Bind pattern
let pPtBind =
    //PtBind.   PatternBind ::= CPattern "<-" CPattern ;
    pipe3 
        (pCPattern .>> ws)
        (skipString "<-" .>> ws)
        (pCPattern .>> ws)
        (fun a b c -> PatternBind.PtBind(a, c))
    <!> "PtBind"

let pPatternBind = 
    pPtBind
    <!> "PatternBind"

let pPatternBindList = 
    //separator nonempty PatternBind ";" ;
    sepBy1 (pPatternBind .>> ws) (skipString ";" .>> ws)   
    <!> "PatternBindList"

// -- Channel patterns
let pCPtVar = 
    //CPtVar.    CPattern ::= VarPattern ;
    (pVarPattern .>> ws) |>>
    (fun x -> CPattern.CPtVar(x))
    <!> "CPtVar"

let pCPtQuote =
    //CPtQuote.  CPattern ::= "@" PPattern3 ;
    pipe2
        (skipString "@")
        (pPPattern3 .>> ws)
        (fun a b -> CPattern.CPtQuote(b))
    <!> "CPtQuote"

let pCPatternList =
    //separator CPattern "," ;
    sepBy (pCPattern .>> ws) (skipString "," .>> ws)
    <!> "CPatternList"

//-- Process patterns
let pPPtVar =
    //PPtVar.    PPattern4 ::= VarPattern ;
    (pVarPattern .>> ws) |>>
    (fun x -> PPattern.PPtVar(x))
    <!> "PPtVar"

let pPPtNil =
    //PPtNil.    PPattern4 ::= "Nil" ;
    (skipString "Nil" .>> ws) |>>
    (fun () -> PPattern.PPtNil)
    <!> "PPtNil"

let pPPtVal =
    //PPtVal.    PPattern4 ::= ValPattern ;
    (pValPattern .>> ws) |>>
    (fun x -> PPattern.PPtVal(x))
    <!> "PPtVal"

let pPPattern4 =
    pPPtVal <|> pPPtNil <|> pPPtVar
    <!> "PPattern4"

let pPPtDrop =
    //PPtDrop.   PPattern3 ::= "*" CPattern ;
    (skipString "*") >>. (pCPattern .>> ws) |>>
    (fun x -> PPattern.PPtDrop(x))
    <!> "PPtDrop"

let pPPtInject =
    //PPtInject. PPattern3 ::= "#" CPattern ;
    (skipString "#") >>. (pCPattern .>> ws) |>>
    (fun x -> PPattern.PPtInject(x))
    <!> "PPtInject"

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
    <!> "PPtOutput"

let pPPattern2 = 
    pPPtOutput
    <!> "PPattern2"

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
    <!> "PPtInput"

let pPPtMatch =
    //PPtMatch.  PPattern1 ::= "match" PPattern "with" [PatternPatternMatch] ;        
    pipe4 
        (skipString "match" .>> ws)
        (pPPattern .>> ws)
        (skipString "with" .>> ws)
        (pPatternPatternMatchList .>> ws)
        (fun a b c d -> PPattern.PPtMatch(b, d))
    <!> "PPtMatch"

let pPPtNew =
    //PPtNew.    PPattern1 ::= "new" [VarPattern] "in" PPattern1 ;
    pipe4
        (skipString "new" .>> ws)
        (pVarPatternList .>> ws)
        (skipString "in" .>> ws)
        (pPPattern1 .>> ws)
        (fun a b c d -> PPattern.PPtNew(b, d))
    <!> "PPtNew"

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
    <!> "PPtConstr"

let pPPtPar =
    //PPtPar.    PPattern  ::= PPattern "|" PPattern1 ;
    sepBy1 (pPPattern .>> ws) (skipString "|" .>> ws) |>>
    (fun x -> PPattern.PPtPar(x))
    <!> "PPtPar"

// -- Variable patterns
let pVarPtVar =
    // VarPtVar.  VarPattern ::= Var ;
    (pVar .>> ws) |>>
    (fun x -> VarPattern.VarPtVar(x))
    <!> "VarPtVar"

let pVarPtWild =
    // VarPtWild. VarPattern ::= "_" ;
    skipString "_" .>> ws |>>
    (fun () -> VarPattern.VarPtWild)
    <!> "VarPtWild"

//-- Values
//-- CArray.  Collect ::= Array ;
//-- CList.   Collect ::= List ;

let pCString =
    //CString. Collect ::= String ;
    (pStringLiteral .>> ws) |>>
    (fun x -> Collect.CString(x))
    <!> "CString"

let pCollect = 
    pCString  // eventually CArray and CList too
    <!> "Collect"

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
    <!> "StructConstr"

let pStruct =
    pStructConstr
    <!> "Struct"

let pEChar =
    //EChar.    Entity   ::= Char ;
    (pCharLiteral .>> ws) |>>
    (fun x -> Entity.EChar(x)) 
    <!> "EChar"

//-- EDate.    Entity   ::= Datetime ;
    
let pEStruct =
    //EStruct.  Entity   ::= Struct ;
    (pStruct .>> ws) |>>
    (fun x -> Entity.EStruct(x))
    <!> "EStruct"

let pECollect =
    //ECollect. Entity   ::= Collect ;
    (pCollect .>> ws) |>>
    (fun x -> Entity.ECollect(x))
    <!> "ECollect"

let pEntity =
    pECollect <|> pEStruct <|> pEChar
    <!> "Entity"

//-- QBool.    Quantity ::= Boolean ;
    
let pQInt =
    //QInt.     Quantity ::= Integer ;
    (pIntegerLiteral .>> ws) |>>
    (fun x -> Quantity.QInt(x))
    <!> "QInt"

let pQDouble =
    //QDouble.  Quantity ::= Double ;
    (pDoubleLiteral .>> ws) |>>
    (fun x -> Quantity.QDouble(x))
    <!> "QDouble"

let pQuantity =
    pQDouble <|> pQInt
    <!> "Quantity"

let pVQuant =
    //VQuant.   Value    ::= Quantity ;
    (pQuantity .>> ws) |>>
    (fun x -> Value.VQuant(x))
    <!> "VQuant"

let pVEnt =
    //VEnt.     Value    ::= Entity ;
    (pEntity .>> ws) |>>
    (fun x -> Value.VEnt(x))
    <!> "VEnt"

let pValue =
    pVEnt <|> pVQuant
    <!> "Value"

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
    <!> "Choice"

let pCBranch = 
    pChoice
    <!> "CBranch"

let pCBranchList =
    //separator nonempty CBranch "" ;
    many pCBranch
    <!> "CBranchList"

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
    <!> "PatternMatch"

let pPMBranch =
    pPatternMatch
    <!> "PMBranch"

let pPMBranchList =
    //separator nonempty PMBranch "" ; 
    many pPMBranch
    <!> "PMBranchList"

//-- Variable binding
let pInputBind =
    //InputBind. Bind ::= CPattern "<-" Chan ;
    pipe3
        (pCPattern .>> ws)
        (skipString "<-" .>> ws)
        (pChan .>> ws)
        (fun a b c -> Bind.InputBind(a, c))
    <!> "InputBind"

//-- Channels
let pCVar =
    //CVar.    Chan ::= Var ;
    (pVar .>> ws) |>>
    (fun x -> Chan.CVar(x))
    <!> "CVar"

let pCQuote =
    //CQuote.  Chan ::= "@" Proc3 ;
    (skipString "@") >>.
    (pProc3 .>> ws) |>>
    (fun x -> Chan.CQuote(x))
    <!> "CQuote"

//-- Processes
let pPNil =
    //PNil.    Proc4 ::= "Nil" ;
    (skipString "Nil" .>> ws) |>>
    (fun () -> Proc.PNil)
    <!> "PNil"

let pPValue =
    //PValue.  Proc4 ::= Value ;
    (pValue .>> ws) |>>
    (fun x -> Proc.PValue(x))
    <!> "PValue"

let pPVar =
    //PVar.    Proc4 ::= Var ;
    (pVar .>> ws) |>>
    (fun x -> Proc.PVar(x))
    <!> "PVar"

let pProc4 =
    pPVar <|> pPValue <|> pPNil
    <!> "Proc4"

let pPDrop = 
    //PDrop.   Proc3 ::= "*" Chan ;
    (skipString "*") >>.
    (pChan .>> ws) |>>
    (fun x -> Proc.PDrop(x))
    <!> "PDrop"

let pPInject =
    //PInject. Proc3 ::= "#" Chan ;
    (skipString "#") >>.
    (pChan .>> ws) |>>
    (fun x -> Proc.PInject(x))
    <!> "PInject"

let pPLift =
    //PLift.   Proc2 ::= Chan "!" "(" [Proc] ")" ;
    pipe2
        //(attempt (pChan .>> (skipString "!" .>> ws)))
        (pChan .>>? (skipString "!" .>> ws))
        (between
            (skipString "(" .>> ws)
            (skipString ")" .>> ws)
            (pProcList .>> ws)
        )
        (fun a b -> Proc.PLift(a, b))
    <!> "PLift"

let pProc2 = 
    pPLift
    <!> "Proc2"

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
    <!> "PInput"

let pPChoice =
    //PChoice. Proc1 ::= "select" "{" [CBranch] "}" ;
    (skipString "select" .>> ws) >>.
    (between
        (skipString "{" .>> ws)
        (skipString "}" .>> ws)
        (pCBranchList .>> ws)
    ) |>>
    (fun x -> Proc.PChoice(x))
    <!> "PChoice"

let pPMatch =
    //PMatch.  Proc1 ::= "match" Proc "with" [PMBranch] ;
    pipe4
        (skipString "match" .>> ws)
        (pProc .>> ws)
        (skipString "with" .>> ws)
        (pPMBranchList .>> ws)
        (fun a b c d -> Proc.PMatch(b, d))
    <!> "PMatch"

let pPNew =
    //PNew.    Proc1 ::= "new" [Var] "in" Proc1 ;
    pipe4
        (skipString "new" .>> ws)
        (pVarList .>> ws)
        (skipString "in" .>> ws)
        (pProc1 .>> ws)
        (fun a b c d -> Proc.PNew(b, d))
    <!> "PNew"

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
    <!> "PConstr"

let pPPar =
    //PPar.    Proc  ::= Proc "|" Proc1 ;
    sepBy1 (pProc .>> ws) (skipString "|" .>> ws) |>>
    (fun x -> Proc.PPar(x))
    <!> "PPar"

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
    <!> "DContr"

let pContr =
    pDContr
    <!> "Contr"

pPPatternRef :=
    choice [
        pPPattern4
        pPPattern3
        pPPattern2
        pPPattern1
    ]
    <!> "PPattern"

pPPattern1Ref :=
    choice [
        pPPtInput
        pPPtMatch 
        pPPtNew 
        pPPtConstr 
    ]
    <!> "PPattern1"

pPPattern3Ref :=
    choice [
        pPPtDrop 
        pPPtInject
    ]
    <!> "PPattern3"

pPPatternListRef :=
    // separator PPattern "," ;
    sepBy (pPPattern .>> ws) (skipString "," .>> ws)
    <!> "PPatternList"

pCPatternRef :=
    choice [
        pCPtVar
        pCPtQuote
    ]
    <!> "CPattern"

pVarPatternRef := 
    choice [
        pVarPtVar
        pVarPtWild
    ]
    <!> "VarPattern"

pVarPatternListRef :=
    // separator VarPattern "," ;
    sepBy (pVarPattern .>> ws) (skipString "," .>> ws)
    <!> "VarPatternList"

pProcRef :=
    choice [
        pProc1
        pProc2
        pProc3
        pProc4
    ]
    <!> "Proc"

pProc1Ref :=
    choice [
        pPInput
        pPChoice
        pPMatch
        pPNew
        pPConstr
    ]
    <!> "Proc1"

pProc3Ref :=
    choice [
        pPDrop
        pPInject
    ]
    <!> "Proc3"

pProcListRef :=
    //separator nonempty Proc "," ;
    sepBy1 (pProc .>> ws) (skipString "," .>> ws)
    <!> "ProcList"

pBindRef :=
    pInputBind
    <!> "Bind"

pBindListRef :=
    // separator nonempty Bind ";" ;
    sepBy1 (pBind .>> ws) (skipString ";" .>> ws)
    <!> "BindList"

pChanRef :=
    choice [
        pCVar
        pCQuote
    ]
    <!> "Chan"
