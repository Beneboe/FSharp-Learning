// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

type And<'a> = And of 'a list
type Or<'a> = Or of 'a list
type Dnf<'a> = Or<And<'a>>

type Exp<'a> = 
    | AndExp of Exp<'a> list
    | OrExp of Exp<'a> list
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

let unboxAnd =
    function
    | And x -> x

let unboxOr =
    function
    | Or x -> x

let distributeAnd (l: And<Or<'a>>) : Or<And<'a>> =
    let l' = unboxAnd l |> List.map unboxOr
    distributeAll l'
    |> List.map And
    |> Or

let rec dnf (exp: Exp<'a>) : Dnf<'a> =
    let flattenAndOfAnds = unboxAnd >> List.collect unboxAnd >> And
    let flattenOrOfOrs = unboxOr >> List.collect unboxOr >> Or

    match exp with
    | AndExp sexp -> 
        sexp
        // Convert sub expressions into dnf
        |> (List.map dnf >> And) 
        // Flip AND of ORs by applying the distributive law
        |> distributeAnd
        // Flatten AND of ANDs
        |> (unboxOr >> List.map flattenAndOfAnds >> Or)
    | OrExp sexp ->
        sexp
        // Convert sub expressions into dnf
        |> (List.map dnf >> Or)
        // Flatten OR of ORs
        |> flattenOrOfOrs
    | Term t -> Or [ And [ t ] ]

[<EntryPoint>]
let main argv =
    printfn "testExp: %A" testExp
    // printfn "distributeOne: %A" (distributeOnce 'a' [['b']; ['c']; ['d']])
    // printfn "distributeAll: %A" (distributeAll [['a'; 'b'; 'c']; ['d'; 'e'; 'f']; ['g';'h'; 'i']])
    printfn "dnf: %A" (dnf testExp)
    0 // return an integer exit code