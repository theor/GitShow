// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Argu
open GitShow.Slide
open GitShow.Runner
open GitShow
open Chessie.ErrorHandling

type Command = Next | Prev | Start | End | Shell
type Args =
//    | Start
    | Presentation of path:string
    | [<AltCommandLineAttribute("-c")>]Command of Command
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | _ -> "??"

let execCommand (p:T) c =
    match c with
    | Next -> next p
    | Prev -> prev p
    | Start -> start p
    | End -> last p
    | _ -> fail Unknown

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let parser = ArgumentParser.Create<Args>()
    let args = parser.ParseCommandLine argv;
    printfn "%A" args
    match args.TryGetResult <@ Command @> with
    | None -> printfn "Interactive mode"
    | Some(c) -> printfn "Command mode: %A" c
    let presFile = args.GetResult(<@ Presentation @>, "pres.yml")
    let res = trial {
        let! p= load(presFile)
        do printfn "%A" p
        let runner = Runner.fromPresentation p
        match args.TryGetResult <@ Command @> with
        | None ->
            printfn "Interactive mode"
            return 0
        | Some(c) ->
            printfn "Command mode: %A" c
            do! execCommand runner c
            return 0
    }
    match res with
    | Fail f ->
        eprintfn "%A" f
        1
    | Pass i -> 0
    | Warn(i,f) ->
        eprintfn "%A" f
        0
