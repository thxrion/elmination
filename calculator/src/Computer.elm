-- todo: account for unary minus and error handling, mb refactor some parts

module Computer exposing (..)

import Html exposing (div, p, text)
import Parser exposing (..)


main =
  case parse "2/(3+4)" of
    Err err ->
      text (Debug.toString err)
    Ok expr ->
      div []
        [ p [] [ text (Debug.toString expr) ]
        , p [] [ text (String.fromFloat (evaluate expr)) ]
        ]

type Expr
  = Integer Int
  | Floating Float
  | Add Expr Expr
  | Subtract Expr Expr
  | Multiply Expr Expr
  | Divide Expr Expr

evaluate : Expr -> Float
evaluate expr =
  case expr of
    Integer n ->
      toFloat n
    Floating n ->
      n
    Add a b ->
      evaluate a + evaluate b
    Multiply a b ->
      evaluate a * evaluate b
    Divide a b ->
      evaluate a / evaluate b
    Subtract a b ->
      evaluate a - evaluate b


parse : String -> Result (List DeadEnd) Expr
parse string =
  run expression string

term : Parser Expr
term =
  oneOf
    [ number
        { int = Just Integer
        , float = Just Floating
        , hex = Nothing
        , octal = Nothing
        , binary = Nothing
        }
    , succeed identity
        |. symbol "("
        |= lazy (\_ -> expression)
        |. symbol ")"
    ]

expression : Parser Expr
expression =
  term
    |> andThen (expressionHelp [])


expressionHelp : List (Expr, Operator) -> Expr -> Parser Expr
expressionHelp revOps expr =
  oneOf
    [ succeed Tuple.pair
        |= operator
        |= term
        |> andThen (\(op, newExpr) -> expressionHelp ((expr,op) :: revOps) newExpr)
    , lazy (\_ -> succeed (finalize revOps expr))
    ]

type Operator =
  AddOperator
  | MultiplyOperator
  | SubtractOperator
  | DivideOperator

operator : Parser Operator
operator =
  oneOf
    [ map (\_ -> AddOperator) (symbol "+")
    , map (\_ -> MultiplyOperator) (symbol "*")
    , map (\_ -> SubtractOperator) (symbol "-")
    , map (\_ -> DivideOperator) (symbol "/")
    ]

finalize : List (Expr, Operator) -> Expr -> Expr
finalize revOps finalExpr =
  case revOps of
    [] ->
      finalExpr

    (expr, MultiplyOperator) :: otherRevOps ->
      finalize otherRevOps (Multiply expr finalExpr)

    (expr, DivideOperator) :: otherRevOps ->
      finalize otherRevOps (Divide expr finalExpr)

    (expr, SubtractOperator) :: otherRevOps ->
      Subtract (finalize otherRevOps expr) finalExpr

    (expr, AddOperator) :: otherRevOps ->
      Add (finalize otherRevOps expr) finalExpr