namespace GitShow

open Chessie.ErrorHandling
open Newtonsoft.Json
/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
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
  type Presentation = T array
  type IImpl =
    abstract member SetSlide: T -> Result<unit, Error>
    abstract member GetCurrent: Presentation -> Result<T*int, Error>

  type GitImpl() =
    let run = ignore << Process.runProcess ignore ignore "git"
    
    let runf args =
        let mutable r = []
        let p = Process.runProcess (fun x -> r <- x :: r) (printfn "ERROR: %s") "git" args
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
        


  let load (path:string): Result<Presentation, Error> =
    let json = System.IO.File.ReadAllText(path)
    
    Chessie.ErrorHandling.Trial.Catch ((fun x -> JsonConvert.DeserializeObject<Presentation>(x))) json
    |> mapFailure (List.map Serialization)

  let save (path:string) (p:Presentation): Result<unit, Error> =
    Chessie.ErrorHandling.Trial.Catch (JsonConvert.SerializeObject >> (fun x -> System.IO.File.WriteAllText(path,x))) p
    |> mapFailure (List.map Serialization)
    

module Runner =
  open Slide

  type T = { presentation: Slide.Presentation; impl:IImpl }
  let fromPresentation p = { presentation=p; impl=GitImpl() }

  let private findIndex (t:T) =
    trial {
      let! (_,i) = t.impl.GetCurrent t.presentation
      return i
    }

  let start (t:T) = t.impl.SetSlide (t.presentation.[0])
  let last (t:T) = t.impl.SetSlide (Array.last t.presentation)
  let next (t:T) =
    trial {
        let! i = findIndex t
        let res = if i + 1 < Array.length t.presentation
                  then t.impl.SetSlide (t.presentation.[i + 1])
                  else warn (Warn "no next slide") ()
        return! res
    }
  let prev (t:T) =
    trial {
        let! i = findIndex t
        let res = if i > 0
                  then t.impl.SetSlide (t.presentation.[i - 1])
                  else warn (Warn "no next slide") ()
        return! res
    }