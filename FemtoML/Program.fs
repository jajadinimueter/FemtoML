module FemtoML.Main

open System
open FParsec

type BinOp = 
    | Eq
    | Lt
    | Gt
    | Add
    | Sub
    | Mul
    | Div

type Expr = 
    | Int of int
    | String of string
    | Ident of string
    | BinOp of Expr * BinOp * Expr
    | Apply of Expr * Expr
    | If of Expr * Expr * Expr
    | Lambda of string * Expr
    | Let of bool * string * string list * Expr * Expr
    | LetStmt of bool * string * Expr

let pInt = puint32 |>> int .>> spaces

let pStringLiteral : Parser<_, unit> =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))
            
let str s = pstring s .>> spaces;;

let keywords = ["if"; "then"; "else"; "let"; "rec"; "in"; "fun"]

let pIdent =
    let isAlphaNum c = isLetter c || isDigit c
    let idStr = many1Satisfy2L 
                    isLetter 
                    isAlphaNum 
                    "identifier" 
                    .>> spaces
    let p state = 
        let reply = idStr state
        if reply.Status = Ok && 
                (List.exists ((=) reply.Result) keywords) then
            Reply(Error, expected "identifier")
        else
            reply
    attempt p .>> spaces

let opp = OperatorPrecedenceParser<Expr, unit, unit>();;

let pExpr = opp.ExpressionParser .>> spaces

let pAtom = 
    choice [
        pInt |>> Int
        pStringLiteral |>> String
        pIdent |>> Ident
        between (str "(") (str ")") pExpr 
    ]

let pAtoms = many1 pAtom |>> fun fs -> 
    List.reduce(fun f g -> Apply(f, g)) fs

let pIf = 
    pipe3 
        (str "if" >>. pExpr) 
        (str "then" >>. pExpr) 
        (str "else" >>. pExpr) 
        (fun p t f -> If(p, t, f))

let pipe6 p1 p2 p3 p4 p5 p6 f =
    pipe4 p1 p2 p3 (tuple3 p4 p5 p6)
          (fun x1 x2 x3 (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)

let pLetStmt = 
    pipe3 
        (str "let" >>. opt (str "rec")) 
        (pIdent .>> str "=") 
        pExpr 
        (fun r ident expr -> 
            LetStmt(match r with
                    | Some(_) -> true
                    | None -> false
                    , ident, expr))
let pLetExpr = 
    pipe5
        ((str "let")) 
        (opt (str "rec")) 
        (pIdent .>> str "=") 
        pExpr 
        (str "in" >>. pExpr) 
        (fun _ r var body rest -> 
            let rBool = 
                match r with
                | Some(_) -> true
                | None -> false
            Let(rBool, var, [], body, rest))

let term = 
    choice [
        pAtoms
        pIf
        pLetExpr
        pipe2 
            (str "fun" >>. pIdent .>> str "->") 
            pExpr (fun par body -> Lambda(par, body))
    ]

opp.TermParser <- term

opp.AddOperator(
    InfixOperator("=", spaces, 1, Associativity.Left, 
                  fun f g -> BinOp(f, Eq, g)))

opp.AddOperator(
    InfixOperator("<", spaces, 1, Associativity.Left, 
                  fun f g -> BinOp(f, Lt, g)))
        
opp.AddOperator(
    InfixOperator(">", spaces, 1,  Associativity.Left, 
                  fun f g -> BinOp(f, Gt, g)))
        
opp.AddOperator(
    InfixOperator("+", spaces, 50, Associativity.Left, 
                  fun f g -> BinOp(f, Add, g)))
        
opp.AddOperator(
    InfixOperator("-", spaces, 50, Associativity.Left, 
                  fun f g -> BinOp(f, Sub, g)))

opp.AddOperator(
    InfixOperator("*", spaces, 100, Associativity.Left, 
                  fun f g -> BinOp(f, Mul, g)))

opp.AddOperator(
    InfixOperator("/", spaces, 100, Associativity.Left, 
                  fun f g -> BinOp(f, Div, g)))

let pStatement = (attempt pExpr <|> attempt pLetStmt) .>> spaces

let parseEndStatement (stream : string) = 
    let parseResult = run (manySatisfy (fun c -> c <> ';') .>> (str ";" .>> spaces .>> str";")) stream in 
        match parseResult with
            | Success(expr, _, _) -> Some expr
            | _ -> None

let parse stream = run (spaces >>. pStatement .>> eof) stream

type Value = 
    | VInt of int
    | VString of string
    | VBool of bool
    | VClosure of string * Map<string, Value ref> * Expr

let rec eval vars = function
    | Int i -> VInt i
    | String i -> VString i
    | Ident var ->
        match Map.tryFind var vars with
           | Some value -> !value
           | None -> failwithf "Unknown variable '%s'" var
    | BinOp(f, op, g) ->
        let left = eval vars f
        let middle = op
        let right = eval vars g
        match left, middle, right with
        | VInt m, Eq, VInt n -> VBool(m = n)
        | VInt m, Lt, VInt n -> VBool(m < n)
        | VInt m, Gt, VInt n -> VBool(m > n)
        | VInt m, Add, VInt n -> VInt(m + n)
        | VInt m, Sub, VInt n -> VInt(m - n)
        | VInt m, Mul, VInt n -> VInt(m * n)
        | VInt m, Div, VInt n -> VInt(m / n)
        | _ -> failwith "Type error"
    | Apply(func, arg) ->
        match eval vars func, eval vars arg with
        | VClosure(var, vars, body), arg -> 
            eval (Map.add var (ref arg) vars) body
        | _ -> failwith "Attempt to apply a non-function value"
    | If(p, t, f) ->
        let evaluatedCondition = eval vars p
        match evaluatedCondition with
        | VBool p -> eval vars (if p then t else f)
        | _ -> failwith "Type error"
    | Lambda(arg, body) -> 
        VClosure(arg, vars, body)
    | Let(true, var, args, body, inClause) ->
        let value = ref(VInt 0)
        let vars = Map.add var value vars
        value := eval vars body
        match !value with
        | VClosure(_,_,_) -> eval vars inClause
        | _ -> failwith "Recursive definition must be a function"
    | Let(false, var, args, body, inClause) ->
        let value = ref (eval vars body) 
        eval (Map.add var value vars) inClause
    | LetStmt(_, _, _) -> failwith "let statement not allowed inside expression"

let rec accumulateStatement s = 
    let line = Console.ReadLine()
    match (s + line) |> parseEndStatement with
        | None -> accumulateStatement (s + line)
        | Some(l) -> l

let rec mainloop state = 
    printf "> "
    let state = 
        accumulateStatement "" |> parse
            |> function
                | Success(expr, _, _) ->
                    try
                        match expr with
                        | LetStmt(false, var, body) -> 
                            let result = eval state body
                            printf "%A\n" result
                            Map.add var (ref result) state
                        | LetStmt(true, var, body) -> 
                            let value = ref(VInt 0)
                            let vars = Map.add var value state
                            value := eval vars body
                            printf "%A\n" value
                            Map.add var value vars
                        | _ -> 
                            let result = eval state expr 
                            printf "%A\n" result
                            state
                    with 
                        | _ as ex -> 
                            printf "%A\n" ex.Message
                            state
                | Failure(str, _, _) -> 
                    printf "%s\n" str
                    state
    (mainloop state)

[<EntryPoint>]
let main args = 
    let initialState = [("true", ref (VBool true)); ("false", ref (VBool false))] |> Map.ofList
    initialState |> mainloop
