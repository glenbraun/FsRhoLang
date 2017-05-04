open System

open Rholang.AST
open Rholang.Parser
open Rholang.CodeGen

open FParsec
open FsUnit

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

    Fuchu.Tests.run tests |> ignore

    0 // return an integer exit code
