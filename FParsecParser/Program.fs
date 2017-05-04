#nowarn "40"

open System

open FParsec
open FParsec.CharParsers

(*


let test p str =
    printfn "%s" str
    System.Diagnostics.Trace.WriteLine(str)

    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

*)
[<EntryPoint>]
let main argv = 

(*
    test pVarPatternList "a ,b            " 
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

    //test cc "c4"

    test pProc "a"

    test pContr "contract Glen (a, b) = { 'a'}"
    test pContr "contract Glen (a, b) = { 'a' }"

    test pContr "contract Glen (a, b) = { a }"
    test pContr "contract Glen (a, b) = { *a }"
    test pContr "contract Glen (a, b) = { #a }"

    test pContr "contract Glen (a, b) = { a!( 1 ) }"
    test pContr "contract Glen (a, b) = { a!( Nil ) }"
    *)

    0 // return an integer exit code
