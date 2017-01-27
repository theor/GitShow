namespace GitShow

open Chessie.ErrorHandling
open Newtonsoft.Json

module Slide = 
  open Process
  open System.Collections.Generic
  
  [<StructuralEquality;StructuralComparison>]
  type T = {
    commit: string;
    command: string option
  }
  let from (id:string) : T = { commit=id; command=None }
  type Error = Unknown | GitError | ModifiedFiles | Serialization of exn | Warn of string
  

module Presentation =
    open Slide
    type Presentation = T array
    type IImpl =
        abstract member SetSlide: T -> Result<unit, Error>
        abstract member GetCurrent: Presentation -> Result<T*int, Error>

    let load (path:string): Result<Presentation, Error> =
        let json = System.IO.File.ReadAllText(path)
    
        Chessie.ErrorHandling.Trial.Catch ((fun x -> JsonConvert.DeserializeObject<Presentation>(x))) json
        |> mapFailure (List.map Serialization)

    let save (path:string) (p:Presentation): Result<unit, Error> =
        Chessie.ErrorHandling.Trial.Catch (JsonConvert.SerializeObject >> (fun x -> System.IO.File.WriteAllText(path,x))) p
        |> mapFailure (List.map Serialization)

module Git =
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