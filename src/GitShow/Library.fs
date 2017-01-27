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
  type Error =
    | Unknown
    | GitError
    | ModifiedFiles
    | Serialization of exn
    | Warn of string
    | Shell of string
  

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
