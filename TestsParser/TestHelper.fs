module TestHelper

open FParsec
open Rholang.Parser
open Rholang.CodeGen

let RoundTripTest contract =
    // Generate code based on the contract AST
    let code = GenerateRholang(contract)

    match run pContr code with
    | Success(result, _, _) ->
        // if parse was successful, check that the AST returned equals what was sent in
        (result = contract)
    | Failure(errorMsg, _, _) ->
        false
