open System
open System.Text

let pch p = function
    | c::cs when p c -> (Some (c.ToString()), cs)
    | chars -> (None, chars)
    
let ch c = pch ((=) c)

let rec internal repeatParse (parser: char list -> string option * char list) (stringBuilder: StringBuilder) chars =
    match parser chars with
    | (Some s, cs) -> stringBuilder.Append(s) |> ignore
                      repeatParse parser stringBuilder cs
    | (None, cs) -> (Some (stringBuilder.ToString()), cs) 

let (!*) parser chars =
    let mutable stringBuilder = StringBuilder()
    repeatParse parser stringBuilder chars

let (!+) (parser: char list -> string option * char list) chars =
    match parser chars with
    | (Some s, cs) -> let mutable stringBuilder = StringBuilder(s)
                      repeatParse parser stringBuilder cs
    | none -> none
    
let (!?) parser chars =
    match parser chars with
    | (Some s, cs) -> (Some s, cs)
    | (None, cs) -> (Some "", cs)
    
let (@>>) parser1 parser2 chars =
    match parser1 chars with
    | (Some s1, cs1) -> match parser2 cs1 with
                        | (Some s2, cs2) -> (Some (s1 + s2), cs2)
                        | _ -> (None, chars)
    | _ -> (None, chars)
    
let pdigits = !+ (pch Char.IsDigit)

let (@|>) (parser: char list -> string option * char list) f chars =
    match parser chars with
    | (Some s, cs) -> (Some (f s), cs)
    | (None ,cs) -> (None, cs)
    
let pfloat = pdigits @>> !? (ch ',' @>> pdigits)
    
"3.14159265" |> List.ofSeq
             |> pfloat
             |> printfn "%A" 
