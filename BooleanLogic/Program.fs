// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type And<'a> = 'a list
type Or<'a> = 'a list

type Exp<'a> = 
    | AndExp of And<Exp<'a>>
    | OrExp of Or<Exp<'a>>
    | Term of 'a

let testExp = AndExp [ OrExp [Term 1; Term 2]; OrExp [ Term 3; Term 4] ]

let rec distribute a b =
    match (a, b) with
    | ([],[]) -> []
    | (xs, []) -> []
    | ([], ys) -> []
    | (x::xs, ys) -> 
        let head = ys |> List.map (fun y -> [x; y])
        head @ (distribute xs ys)


let rec distributeOnce a b =
    match b with
    | [] -> []
    | xs -> xs |> List.map (fun x -> [a] @ x)

let rec distributeAll l =
    match l with
    | [] -> []
    | [x] -> x |> List.map (fun a -> [a])
    | x::xs ->
        match x with
        | [] -> []
        | _ -> x |> List.collect (fun a -> distributeOnce a (distributeAll xs))

let rec dnf (exp: Exp<'a>) : Or<And<'a>> =
    match exp with
    | AndExp sexp -> 
        sexp
        // Convert sub expressions into dnf
        |> List.map dnf
        // Flip AND of ORs by applying the distributive law
        |> distributeAll
        // The output is an OR of ANDs of ANDs 
        // Flatten AND of ANDs
        |> List.map (List.collect id)
    | OrExp sexp ->
        sexp
        // Convert sub expressions into dnf
        |> List.map dnf
        // Flatten OR of ORs
        |> List.collect id
    | Term t -> [ [ t ] ]

[<EntryPoint>]
let main argv =
    printfn "testExp: %A" testExp
    // printfn "distributeOne: %A" (distributeOnce 'a' [['b']; ['c']; ['d']])
    // printfn "distributeAll: %A" (distributeAll [['a'; 'b'; 'c']; ['d'; 'e'; 'f']; ['g';'h'; 'i']])
    printfn "dnf: %A" (dnf testExp)
    0 // return an integer exit code