namespace SML

module slp =

    type Id = string
    type BinaryOperator = Plus | Minus | Times | Div
    type Statement =
        | CompoundStatement of Statement * Statement
        | AssignStatement of Id * Expression
        | PrintStatement of Expression list
    and Expression = 
        | NumberExpression of int
        | IdExpression of Id
        | OperationExpression of Expression * BinaryOperator * Expression
        | SequenceExpression of Statement * Expression
    type Context = Map<Id, Expression>

    let introductionProgram =  
        CompoundStatement(
            AssignStatement(
                "a", 
                OperationExpression(
                    NumberExpression 5, Plus, NumberExpression 3)
            ),
            CompoundStatement(
                AssignStatement(
                    "b", 
                    SequenceExpression(
                        PrintStatement [
                            IdExpression "a";
                            OperationExpression(
                                IdExpression "a", 
                                Minus, 
                                NumberExpression 1
                            )
                        ],
                        OperationExpression(
                            NumberExpression 10,
                            Times,
                            IdExpression "a"
                        )
                    ))
                ,
                PrintStatement[
                    IdExpression "b"
                    ]
                )
            )
