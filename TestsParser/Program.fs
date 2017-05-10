open System

open Rholang.AST
open Rholang.Parser
open Rholang.CodeGen

open FParsec
open FsUnit
open RhoProgramGenerator

let tests = 
    Fuchu.TestList [
        Fuchu.TestCase ContractTests.``SingleCharName, Empty CPattern list, Proc.PNil Proc``
        Fuchu.TestCase ContractTests.``MultiCharName, Empty CPattern list, Proc.PNil Proc``
        Fuchu.TestCase ContractTests.``MultiCharName, one item CPattern.CPtVar:VarPattern.VarPtVar:Var.VarValue list, Nil Proc``
        Fuchu.TestCase ContractTests.``MultiCharName, one item CPattern.CPtVar:VarPattern.VarPtWild list, Nil Proc``
        Fuchu.TestCase ContractTests.``MultiCharName, one item CPattern.CPtQuote:PPattern.PPtDrop:CPattern.CPtVar:VarPattern.VarPtVar:Var.VarValue list, Nil Proc``
    ]


[<EntryPoint>]
let main argv = 
//    let tests = Fuchu.TestCase ContractTests.``MultiCharName, one item CPattern.CPtQuote:PPattern.PPtConstr:Name.NameValue list, Nil Proc``
    let pp = 
        (between
            (skipString "{" .>> ws)
            (skipString "}" .>> ws)
            (pProc4 .>> ws)
        )

    let code = "contract N () = {v { for (v <- v) {select {case v <- v=> {match Nil with v => {Nil | Nil} } } } } }"
    match run pContr code with
    | Success(result, _, _) ->
        // if parse was successful, check that the AST returned equals what was sent in
        ()
    | Failure(errorMsg, _, _) ->
        failwith errorMsg


    let contracts = GenerateContr()
    let ht = new System.Collections.Generic.HashSet<obj>()
    use fout = System.IO.File.CreateText("..\\..\\programs.rho")
    use fobj = System.IO.File.CreateText("..\\..\\programs.txt")

    for contract in contracts do
        PrintAST contract fobj

        if not (ht.Add(contract)) then
            printfn "Duplicate %A" contract

        let code = GenerateRholang(contract)

        fobj.WriteLine((sprintf "%A" contract).Replace("\r", "").Replace("\n",""))

        let passed = TestHelper.RoundTripTest contract
        fout.WriteLine (sprintf "%b:%s" passed code )
        printfn "%b:%s" passed code

        if not passed then
            printfn "stop"


        if ht.Count >= 100 then
            printfn "done"

    fout.Flush()
    fout.Close()

    fobj.Flush()
    fobj.Close()

(*
    for contract in contracts do
        //let code = GenerateRholang(contract)
        printfn "Contract: %A" contract
        //printfn "Code: %s" code

        let success = TestHelper.RoundTripTest contract
        if success then
            printfn "Round trip passed"
        else
            failwith "Test failed"
            *)

    //Fuchu.Tests.run tests |> ignore

    0 // return an integer exit code
