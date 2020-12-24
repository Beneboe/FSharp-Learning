// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type And<'a> = And of 'a list
type Or<'a> = Or of 'a list

type Exp<'a> = 
    | AndExp of And<Exp<'a>>
    | OrExp of Or<Exp<'a>>
    | Term of 'a

let testExp = AndExp (And [ OrExp (Or [Term 1; Term 2]); OrExp (Or [ Term 3; Term 4]) ])

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

let unboxAnd =
    function
    | And x -> x

let unboxOr =
    function
    | Or x -> x

let distributeAnd (l: And<Or<'a>>) : Or<And<'a>> =
    let l' = unboxAnd l |> List.map (unboxOr)
    distributeAll l'
    |> List.map (And)
    |> Or

let rec dnf (exp: Exp<'a>) : Or<And<'a>> =
    match exp with
    | AndExp sexp -> 
        sexp
        |> unboxAnd
        // Convert sub expressions into dnf
        |> List.map dnf
        |> And
        // Flip AND of ORs by applying the distributive law
        |> distributeAnd
        // Flatten AND of ANDs
        |> unboxOr
        |> List.map (unboxAnd >> List.collect (unboxAnd) >> And)
        |> Or
    | OrExp sexp ->
        sexp
        |> unboxOr
        // Convert sub expressions into dnf
        |> List.map dnf
        // Flatten OR of ORs
        |> List.collect unboxOr
        |> Or
    | Term t -> Or [ And [ t ] ]

[<EntryPoint>]
let main argv =
    printfn "testExp: %A" testExp
    // printfn "distributeOne: %A" (distributeOnce 'a' [['b']; ['c']; ['d']])
    // printfn "distributeAll: %A" (distributeAll [['a'; 'b'; 'c']; ['d'; 'e'; 'f']; ['g';'h'; 'i']])
    printfn "dnf: %A" (dnf testExp)
    0 // return an integer exit code