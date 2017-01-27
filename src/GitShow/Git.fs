namespace GitShow

module Git =
    open Chessie.ErrorHandling
    open Slide
    open Presentation
    type GitImpl() =
        let run = ignore << Process.runProcess ignore ignore "git"
    
        let runf args =
            let mutable r = []
            let _ = Process.runProcess (fun x -> r <- x :: r) (printfn "ERROR: %s") "git" args
            Seq.toArray r

        interface IImpl with
            override x.SetSlide(s:T) =
                run ["checkout"; "-q"; s.commit]
                match s.command with
                | _ -> ()
                ok ()
            override x.GetCurrent(p:Presentation) =
                trial {
                    let! curId = runf ["rev-parse"; "HEAD"] |> Array.tryItem 0 |> failIfNone GitError
                    let! i = p |> Array.tryFindIndex (fun s -> s.commit = curId) |> failIfNone Unknown
                    return (p.[i],i)
                }