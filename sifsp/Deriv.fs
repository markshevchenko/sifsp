module Deriv

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns


let make_sum (left: Expr) (right: Expr) =
    let left = Expr.Cast<float> left
    let right = Expr.Cast<float> right
    <@ %left + %right @> :> Expr
    
let inline make_prod (left: Expr) (right: Expr) =
    let left = Expr.Cast<float> left
    let right = Expr.Cast<float> right
    <@ %left * %right @> :> Expr


let deriv (exp: Expr) =
    match exp with
    | Lambda(arg, body) ->
        let rec d exp =
            match exp with
            | Int32(_) ->
                Expr.Value(0.0)
            | Var(var) ->
                if var.Name = arg.Name
                then Expr.Value(1.0)
                else Expr.Value(0.0)
            | Double(_) ->
                Expr.Value(0.0)
            | SpecificCall <@ (+) @> (None, _, [left; right]) ->
                make_sum (d left) (d right)
            | SpecificCall <@ (*) @> (_, _, [left; right]) ->
                let left = Expr.Cast<float> left
                let right = Expr.Cast<float> left
                make_sum (make_prod left (d right)) (make_prod (d left) right)
            | _ -> exp

        d body
    | _ -> failwith "Expr.Lambda expected"