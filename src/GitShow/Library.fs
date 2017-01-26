namespace GitShow

open Chessie.ErrorHandling
open YamlDotNet.Serialization
/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Slide = 
  open Process
  
  [<StructuralEquality;StructuralComparison>]
  type T = {
    commit: string;
    command: string option
  }
  let from (id:string) : T = { commit=id; command=None }
  type Error = Unknown | ModifiedFiles | Serialization of exn | Warn of string
  type IImpl =
    abstract member SetSlide: T -> Result<unit, Error>
    abstract member GetCurrent: Unit -> Result<T, Error>

  type GitImpl() =
    let runf = ignore << Process.runProcess ignore ignore "git"
    interface IImpl with
        override x.SetSlide(s:T) =
          runf ["checkout"; "-q"; s.commit]
          match s.command with
          | _ -> ()
          ok ()
        override x.GetCurrent() = fail Unknown
        

  type Presentation = T array

  let load (path:string): Result<Presentation, Error> =
    use f:System.IO.TextReader = upcast(System.IO.File.OpenText(path))
    
    let d = Deserializer()
    Chessie.ErrorHandling.Trial.Catch (fun (x:System.IO.TextReader) -> d.Deserialize<Presentation>(x)) f
    |> mapFailure (List.map Serialization)

  let save (path:string) (p:Presentation): Result<unit, Error> =
    use f:System.IO.TextWriter = upcast(new System.IO.StreamWriter(path))
    
    let d = Serializer()
    Chessie.ErrorHandling.Trial.Catch (fun (x:System.IO.TextWriter) -> d.Serialize(x, p)) f
    |> mapFailure (List.map Serialization)
    

module Runner =
  open Slide

  type T = { presentation: Slide.Presentation; impl:IImpl }
  let fromPresentation p = { presentation=p; impl=GitImpl() }

  let private findIndex (t:T) =
    trial {
      let! cur = t.impl.GetCurrent()
      let i = Array.findIndex((=) cur) t.presentation
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