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

    let contracts = GenerateContr()
    let ht = new System.Collections.Generic.HashSet<obj>()
    use fout = System.IO.File.CreateText("..\\..\\programs.rho")
    use fobj = System.IO.File.CreateText("..\\..\\programs.txt")

    for contract in contracts do
        if not (ht.Add(contract)) then
            printfn "%A" contract

        let code = GenerateRholang(contract)
        fout.WriteLine code 

        fobj.WriteLine((sprintf "%A" contract).Replace("\r", "").Replace("\n",""))

        if ht.Count >= 1000 then
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
