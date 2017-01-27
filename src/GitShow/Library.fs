namespace GitShow

open Chessie.ErrorHandling
open Newtonsoft.Json

module Slide = 
  open Process
  open System.Collections.Generic
  
  [<StructuralEquality;StructuralComparison>]
  type T = {
    commit: string;
    command: string option;
    [<JsonIgnore>] message: string option;
    [<JsonIgnore>] index: int
  }
  let format (t:T): string =
    let cmd = t.command |> Option.map (fun x -> sprintf "\tcmd:'%s'\n" x)
    sprintf "%i\t%s\n\t%s\n%s" t.index (defaultArg t.message "<?>") t.commit (defaultArg cmd "")
  let from (id:string) : T = { commit=id; command=None; message=None; index=(-1) }
  type Error =
    | Unknown
    | GitError
    | ModifiedFiles
    | Serialization of exn
    | Parsing of string
    | Warn of string
    | Shell of string
  

module Presentation =
    open Slide
    type Presentation = T array
    type IImpl =
        abstract member SetSlide: T -> Result<unit, Error>
        abstract member GetMessage: T -> Result<string, Error>
        abstract member GetCurrent: Presentation -> Result<T*int, Error>

    let load (impl:IImpl) (path:string): Result<Presentation, Error> =
        let json = System.IO.File.ReadAllText(path)
    
        let r = Chessie.ErrorHandling.Trial.Catch ((fun x -> JsonConvert.DeserializeObject<Presentation>(x))) json
                |> mapFailure (List.map Serialization)
        let fetch i x =
            trial {
                let! msg = impl.GetMessage x |> mapFailure id
                return { x with index = i; message = Some(msg) }
            }
        trial {
            let! r = r
            let s = r |> Array.mapi fetch
            let! slides = s  |> Seq.ofArray |> collect
            return slides |> Seq.toArray
        }

    let save (path:string) (p:Presentation): Result<unit, Error> =
        Chessie.ErrorHandling.Trial.Catch (JsonConvert.SerializeObject >> (fun x -> System.IO.File.WriteAllText(path,x))) p
        |> mapFailure (List.map Serialization)
