// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Argu
open GitShow.Slide
open GitShow.Runner
open GitShow
open Chessie.ErrorHandling

type Command = Next | Prev | Start | End | Shell | Cur
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
    | Cur ->
        trial {
            let! cur = p.impl.GetCurrent p.presentation
            do printfn "Current: %A" cur
            return ()
        }
    | _ -> fail Unknown

let runInteractive (r:Runner.T) =
    let mutable stop = false
    while not stop do
        printf "> "
        let s = System.Console.ReadLine()
        if s.Chars 0 = '!'
        then
            let pi = Process.runProcess(printfn "%s")(eprintfn "%s") "cmd" ["/c";s.Substring 1]
            ()
        else
            let res = match s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) with
                    | [|"n"|] | [|"next"|] -> execCommand r Next
                    | [|"p"|] | [|"prev"|] -> execCommand r Prev
                    | [|"s"|] | [|"start"|] -> execCommand r Start
                    | [|"e"|] | [|"end"|] -> execCommand r End
                    | [|"c"|] | [|"current"|] -> execCommand r Cur
                    | [|"l"|] | [|"list"|] -> printfn "%A" r; ok ()
                    | [| "exit" |] -> stop <- true; ok ()
                    | x -> eprintfn "Unknown command: %A" x; ok ()
                
            match res with
            | Fail f ->
                eprintfn "%A" f
            | Pass i -> ()
            | Warn(i,f) ->
                eprintfn "%A" f
    0

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let parser = ArgumentParser.Create<Args>()
    let args = parser.ParseCommandLine argv;
    printfn "%A" args
    match args.TryGetResult <@ Command @> with
    | None -> printfn "Interactive mode"
    | Some(c) -> printfn "Command mode: %A" c
    let presFile = args.GetResult(<@ Presentation @>, "pres.json")
    let res = trial {
        let! p= Presentation.load(presFile)
        do printfn "%A" p
        let runner = Runner.fromPresentation p
        match args.TryGetResult <@ Command @> with
        | None ->
            printfn "Interactive mode"
            return runInteractive runner
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
