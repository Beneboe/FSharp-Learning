// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type And<'a> = 'a list
type Or<'a> = 'a list

type Exp<'a> = 
    | And of Exp<'a> list
    | Or of Exp<'a> list
    | Term of 'a

let testExp = And [ Or [Term 1; Term 2]; Or [ Term 3; Term 4] ]

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

let rec dnf (exp: Exp<'a>) : Exp<'a> =
    match exp with
    | And sexp -> 
        let p = sexp
                |> List.map dnf
                |> List.map (fun ssexp -> match ssexp with Or s -> s)

        distributeAll p
        |> List.map (fun a -> a |> List.collect (fun b -> match b with (And c) -> c))
        |> List.map (And)
        |> Or
    | Or sexp ->
        sexp
        |> List.map dnf 
        |> List.collect (fun ssexp -> match ssexp with  Or a -> a)
        |> Or
    | Term t -> Or [ And [ Term t ] ]

[<EntryPoint>]
let main argv =
    printfn "testExp: %A" testExp
    // printfn "distributeOne: %A" (distributeOnce 'a' [['b']; ['c']; ['d']])
    // printfn "distributeAll: %A" (distributeAll [['a'; 'b'; 'c']; ['d'; 'e'; 'f']; ['g';'h'; 'i']])
    printfn "dnf: %A" (dnf testExp)
    0 // return an integer exit code