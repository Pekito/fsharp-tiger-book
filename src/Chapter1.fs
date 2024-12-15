namespace Source

module ChapterOne =
    open SML.slp
    let parseNumber exp =
        match exp with
        | NumberExpression n -> n
        | _ -> failwith "Pau"
    let rec evalStatement statement (context: SML.slp.Context) =
        match statement with
        | PrintStatement expList ->
            expList 
            |> List.map (fun exp -> printfn "Print: %A" (evalExpression exp context))
            |> ignore
            
            let lastExpression = expList |> List.last |> (fun x -> evalExpression x context)
            (lastExpression, context)
            
        | AssignStatement (id, expression) ->
            let value = evalExpression expression context
            let newContext = context |> Map.add id value
            (value, newContext)

        | CompoundStatement (st1, st2) ->
            let (_, ctx) = evalStatement st1 context
            evalStatement st2 ctx
    and evalExpression exp context =
        match exp with 
        | NumberExpression n -> NumberExpression n
        | IdExpression id -> Map.find id context
        | OperationExpression (exp1, operator, exp2) ->
            let n1 = evalExpression exp1 context
            let n2 = evalExpression exp2 context
            let n1 = parseNumber n1
            let n2 = parseNumber n2 
            match operator with
            | Plus -> NumberExpression (n1 + n2)
            | Minus -> NumberExpression (n1 - n2)
            | Times -> NumberExpression (n1 * n2)
            | Div -> NumberExpression (n1 / n2)
        | SequenceExpression (st, exp) ->
            let (_, ctx) = evalStatement st context
            evalExpression exp ctx
            