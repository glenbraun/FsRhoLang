open System

open Rholang.AST
open Rholang.Parser
open Rholang.CodeGen

open FParsec
open RhoProgramGenerator

[<EntryPoint>]
let main argv = 
    let code = ""

    // set 'code' above to do one test
    if code.Length > 0 then
        match run pVPtStruct code with
        | Success(result, _, _) ->
            // if parse was successful, check that the AST returned equals what was sent in
            ()
        | Failure(errorMsg, _, _) ->
            failwith errorMsg

    else
        // Use a HashSet to ensure the program generator is generating unique programs
        let ht = new System.Collections.Generic.HashSet<obj>()

        // Text files for the code and the AST tree
        use frho = System.IO.File.CreateText("..\\..\\programs.rho")
        use ftree = System.IO.File.CreateText("..\\..\\programs.ast")

        Seq.takeWhile ( 
            fun contract -> 
                if not (ht.Add(contract)) then
                    failwith "Duplicate contract"
                else
                    let code = GenerateRholang(contract)
                    let passed = TestHelper.RoundTripTest contract

                    frho.WriteLine (sprintf "%b:%s" passed code )
                    printfn "%b:%s" passed code

                    if not passed then
                        PrintAST contract ftree
                        failwith "Test failed"
                    else
                        // Only run this many tests
                        ht.Count <= 1000
            ) 
            (GenerateContr())
            |> Seq.iter (ignore)

        frho.Flush()
        frho.Close()

        ftree.Flush()
        ftree.Close()

    0 // return an integer exit code
