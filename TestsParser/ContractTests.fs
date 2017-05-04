module ContractTests

open Rholang.AST
open FsUnit
open TestHelper

let ``SingleCharName, Empty CPattern list, Proc.PNil Proc``() =
    let contract = Contr.DContr(Name.NameValue("C"), [], Proc.PNil)
    let success = RoundTripTest contract
    success |> should equal true

let ``MultiCharName, Empty CPattern list, Proc.PNil Proc``() = 
    let contract = Contr.DContr(Name.NameValue("Cmulti"), [], Proc.PNil)
    let success = RoundTripTest contract
    success |> should equal true

let ``MultiCharName, one item CPattern.CPtVar:VarPattern.VarPtVar:Var.VarValue list, Nil Proc``() = 
    let contract = Contr.DContr(Name.NameValue("Cmulti"), [CPattern.CPtVar(VarPattern.VarPtVar(Var.VarValue("a")))], Proc.PNil)
    let success = RoundTripTest contract
    success |> should equal true

let ``MultiCharName, one item CPattern.CPtVar:VarPattern.VarPtWild list, Nil Proc``() = 
    let contract = Contr.DContr(Name.NameValue("Cmulti"), [CPattern.CPtVar(VarPattern.VarPtWild)], Proc.PNil)
    let success = RoundTripTest contract
    success |> should equal true

let ``MultiCharName, one item CPattern.CPtQuote:PPattern.PPtDrop:CPattern.CPtVar:VarPattern.VarPtVar:Var.VarValue list, Nil Proc``() = 
    let contract = Contr.DContr(Name.NameValue("Cmulti"), [CPattern.CPtQuote(PPattern.PPtDrop(CPattern.CPtVar(VarPattern.VarPtVar(Var.VarValue("v1")))))], Proc.PNil)
    let success = RoundTripTest contract
    success |> should equal true
