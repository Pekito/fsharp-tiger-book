open SML.slp
open Source.ChapterOne

let expression = evalStatement introductionProgram Map.empty

printfn "Final: %A" expression