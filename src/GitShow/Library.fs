namespace GitShow

open Chessie.ErrorHandling
/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Library = 
  

  type Slide = {
    commit: string;
    command: string
  }
  type GitError = ModifiedFiles
  type IImpl =
    abstract member Checkout: Slide -> Result<string, GitError>
  type GitImpl =
    interface IImpl with
        override x.Checkout(s:Slide) = ok s.commit
        
  type Presentation = Slide[]

  /// Returns 42
  ///
  /// ## Parameters
  ///  - `num` - whatever
  let hello num = 42
