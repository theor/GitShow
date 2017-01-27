// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Argu
open Logary
open GitShow.Slide
open GitShow.Runner
open GitShow
open Chessie.ErrorHandling

type Command =
    | [<AltCommandLineAttribute("n")>] Next
    | Prev | Start | End | Shell | Cur | List

type InteractiveArgs = | [<MainCommandAttribute>] ICommand of Command
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | _ -> "??"

type Args =
//    | Start
    | Presentation of path:string
    | [<AltCommandLineAttribute("-c")>]Command of Command
    | Log of LogLevel
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | _ -> "??"


let execCommand (p:T) c =
    let logger = Logging.getCurrentLogger ()
    match c with
    | Next -> next p
    | Prev -> prev p
    | Start -> start p
    | End -> last p
    | Cur ->
        trial {
            let! (cur,_) = p.impl.GetCurrent p.presentation
            printfn "Current: %A" (Slide.format cur)
            return ()
        }
    | List -> p.presentation |> Array.iter (printfn "%s" << Slide.format); ok ()
    | _ -> fail Unknown

let runInteractive (r:Runner.T) =
    let logger = Logging.getCurrentLogger()
    let mutable stop = false
    while not stop do
        printf "> "
        let s = System.Console.ReadLine()
        let res =
            if s.Length = 0 then ok ()
            else if s.Chars 0 = '!'
            then
                let pi =
                    Process.runProcess
                        (Message.eventDebugf "%s" >> Logger.logSimple logger)
                        (Message.eventDebugf "%s" >> Logger.logSimple logger)
                        "cmd" ["/c";s.Substring 1]
                if pi = 0 then ok() else fail (Error.Shell "error during cmd")
            else
                let parser = ArgumentParser.Create<InteractiveArgs>()
                trial {
                    let! args = Trial.Catch (fun a -> parser.ParseCommandLine a) [| s |] |> mapFailure (List.map (string >> Parsing))
                    let! c = args.TryGetResult(<@ ICommand @>) |> failIfNone (sprintf "Unknown command: %A" s |> Parsing)
                    return! execCommand r c
                }
//                match s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) with
//                | [|"n"|] | [|"next"|] -> execCommand r Next
//                | [|"p"|] | [|"prev"|] -> execCommand r Prev
//                | [|"s"|] | [|"start"|] -> execCommand r Start
//                | [|"e"|] | [|"end"|] -> execCommand r End
//                | [|"c"|] | [|"current"|] -> execCommand r Cur
//                | [|"l"|] | [|"list"|] -> r.presentation |> Array.iter (printfn "%s" << Slide.format); ok ()
//                | [| "exit" |] -> stop <- true; ok ()
//                | x -> eprintfn "Unknown command: %A" x; ok ()
                
        match res with
        | Fail f ->
            eprintfn "%A" f
        | Pass i -> ()
        | Warn(i,f) ->
            eprintfn "%A" f
    0

open Logary.Logging
open Logary.Targets
open Logary.Configuration
open Logary.Formatting.MessageParts

let format (m:Message): string =
    let nl = System.Environment.NewLine
    let ending = System.Environment.NewLine
    let level = string (caseNameOf m.level).[0]
    // https://noda-time.googlecode.com/hg/docs/api/html/M_NodaTime_OffsetDateTime_ToString.htm
//    let time = formatTimestamp m.timestampTicks
    let body = formatValueShallow m
    let name = m.name.ToString()
    let fields = (if Map.isEmpty m.fields then "" else formatFields nl m.fields)
    let context = (if Map.isEmpty m.context then "" else formatContext nl m.context)
    sprintf "%s: %s [%s]%s%s%s" level body name fields context ending

[<EntryPoint>]
let main argv =
    try
        let parser = ArgumentParser.Create<Args>()
        let args = parser.ParseCommandLine argv;

        let logLevel = args.GetResult(<@ Log @>, LogLevel.Info)
        use logary =
            withLogaryManager "GitShow" (
                withTargets [ Console.create {
                    Console.empty with
                        formatter = {
                                      new Formatting.StringFormatter with
                                        member x.format m = format m
                        }
                } "console" ] >>
                withRules [ Rule.createForTarget "console" |> Rule.setLevel logLevel ]
            ) |> Hopac.Hopac.run
        
        let logger = Logging.getCurrentLogger ()
        Message.eventDebugf "%A" args |> Logger.logSimple logger
       
        let presFile = args.GetResult(<@ Presentation @>, "pres.json")
        let res = trial {
            let impl = Git.GitImpl()
            let! p= Presentation.load impl presFile
            Message.eventVerbosef "Presentation: %A" p |> Logger.logSimple logger
            let runner = Runner.fromPresentation p impl
            match args.TryGetResult <@ Command @> with
            | None ->
                Message.eventVerbose "Interactive mode" |> Logger.logSimple logger
                return runInteractive runner
            | Some(c) ->
                Message.eventVerbosef "Command mode: %A" c |> Logger.logSimple logger
                do! execCommand runner c
                return 0
        }
        let flush() = logary.flushPending(NodaTime.Duration.FromSeconds 1L) |> Hopac.Hopac.run
        match res with
        | Pass i -> 0
        | Fail f ->
            f |> List.iter (Message.eventErrorf "%A" >> Logger.logSimple logger)
            flush()
            1
        | Warn(i,f) ->
            f |> List.iter (Message.eventWarnf "%A" >> Logger.logSimple logger)
            flush()
            0
    with
        | e -> eprintfn "%A" e; 1
