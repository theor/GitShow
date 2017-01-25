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
  

  type T = {
    commit: string;
    command: string option
  }
  let from (id:string) : T = { commit=id; command=None }
  type Error = Unknown | ModifiedFiles | Serialization of exn
  type IImpl =
    abstract member SetSlide: T -> Result<string, Error>
    abstract member GetCurrent: Unit -> Result<T, Error>

  type GitImpl() =
    let runf = ignore << Process.runProcess ignore ignore "git"
    interface IImpl with
        override x.SetSlide(s:T) =
          runf ["checkout"; "-q"; s.commit]
          match s.command with
          | _ -> ()
          ok s.commit
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
    

  /// Returns 42
  ///
  /// ## Parameters
  ///  - `num` - whatever
  let hello num = 42
