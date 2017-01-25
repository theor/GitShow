module GitShow.Tests

open Chessie.ErrorHandling
open GitShow.Process
open NUnit.Framework
open System.IO
open GitShow.Slide

let inline assertSuccess r = failed r |> Assert.False

let runf args =
    let mutable r = []
    let p = Process.runProcess (fun x -> r <- x :: r) (printfn "ERROR: %s") "git" args
    Seq.toArray r

let testRepo () =
    let rec setAttributes (di:DirectoryInfo) =
        if di.Exists then
            di.Attributes <- FileAttributes.Normal
            di.GetFiles() |> Array.iter (fun f -> f.Attributes <- FileAttributes.Normal)
            di.GetDirectories() |> Array.iter setAttributes
    let path = Path.Combine [|(Path.GetFullPath(".")); "test"|]
    path |> DirectoryInfo |> setAttributes
    if Directory.Exists path then Directory.Delete(path, true)
    Assert.DoesNotThrow (fun () -> ignore <| Directory.CreateDirectory(path))
    printfn "Cur path %s" path
    Directory.SetCurrentDirectory path

    let log = ref false
    let logIf label = (fun s -> if !log then printfn "%s %s" label s else ignore s)
    let run = ignore << Process.runProcess (logIf "OUT") (logIf "ERROR") "git"
    run ["init"]
    File.WriteAllText("test1.txt", "line1")
    run ["status"]
    run ["add"; "."]
    run ["commit"; "-amStep1"]
    File.WriteAllText("test1.txt", "line1\nline2")
    run ["commit"; "-amStep2"]
    File.WriteAllText("test2.txt", "line1\nline2")
    run ["add"; "."]
    run ["commit"; "-amStep3"]
    let commits = runf ["log"; "--pretty=format:\"%H\""]
    printfn "Commits: %A" commits
    let pres = commits |> Array.map Slide.from
    printfn "Pres: %A" pres
    Slide.save "pres.yml" pres |> assertSuccess
    pres

[<Test>]
let ``setup repo`` () =
    let p = testRepo()
    let nthId n = (Array.item n p).commit
    let r:IImpl = upcast GitImpl()
    r.SetSlide (p.[0]) |> assertSuccess
    let curCommit() = runf ["rev-parse"; "HEAD"] |> Array.item 0
    printfn "Currently at: %A" (curCommit())
    Assert.AreEqual(nthId 0, curCommit())
    r.SetSlide (Array.last p) |> assertSuccess
    Assert.AreEqual(nthId (Array.length p - 1), curCommit())
//    r.
    ()