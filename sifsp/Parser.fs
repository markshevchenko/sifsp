module Parser

open System
open System.Globalization
open System.Text

let pch predicate = function
    | c::cs when predicate c -> Some (c.ToString()), cs
    | chars -> None, chars
    
let ch c = pch ((=) c)

let rec internal repeat parser accumulator combine chars =
    match parser chars with
    | Some s, cs -> repeat parser (combine accumulator s) combine cs
    | None, cs -> accumulator, cs

let rep1 (parser: char list -> string option * char list) chars =
    match parser chars with
    | Some s, cs -> let mutable stringBuilder = StringBuilder(s)
                    let stringBuilder, cs = repeat parser stringBuilder (fun stringBuilder -> stringBuilder.Append) cs
                    Some (stringBuilder.ToString()), cs
    | none -> none
    
let opt parser defaultValue chars =
    match parser chars with
    | Some s, cs -> Some s, cs
    | None, cs -> Some defaultValue, cs
    
let (.>>.) (parser1: char list -> 'a option * char list)
           (parser2: char list -> 'b option * char list)
           (f: 'a -> 'b -> 'c)
           (chars: char list): ('c option * char list) =
    match parser1 chars with
    | Some s1, cs1 -> match parser2 cs1 with
                      | Some s2, cs2 -> Some (f s1  s2), cs2
                      | _ -> None, chars
    | _ -> None, chars
    
let map (parser: char list -> 'a option * char list) (f: 'a -> 'b) chars =
    let result, cs = parser chars
    Option.map f result, cs
    
let digits = rep1 (pch Char.IsDigit)
let frac = opt ((ch '.' .>>. digits) (+)) ""
let number = map ((digits .>>. frac) (+)) (fun s -> Convert.ToDouble(s, CultureInfo.InvariantCulture))

let (<|>) parser1 parser2 chars =
    match parser1 chars with
    | Some result, cs -> Some result, cs
    | None, cs -> parser2 chars
    
let star = (ch '*' .>>. number) (fun _ -> id)
let slash = (ch '/' .>>. number) (fun _ x -> 1.0/x)

let factors chars =
    match number chars with
    | Some value, cs -> let value, cs = repeat (star <|> slash) value (*) cs
                        Some value, cs
    | None, cs -> None, cs
    
let plus = (ch '+' .>>. factors) (fun _ -> id)
let minus = (ch '-' .>>. factors) (fun _ x -> -x)

let terms chars =
    match factors chars with
    | Some value, cs -> let value, cs = repeat (plus <|> minus) value (+) cs
                        Some value, cs
    | None, cs -> None, cs
