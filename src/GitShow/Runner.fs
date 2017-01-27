namespace GitShow

module Runner =

    open Slide
    open Presentation
    open Git
    open Chessie.ErrorHandling

    type T = { presentation: Presentation.Presentation; impl:IImpl }
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
