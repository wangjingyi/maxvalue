module Interpret
 
let open_paren = "("
let close_paren = ")"
let plus = "+"
let times = "*"

type Interpreter(tokens : string seq) =
 
    let pos = ref 0
 
    member this.move = 
        pos := !pos + 1
 
    member private this.is_done = 
        !pos >= Seq.length tokens
 
    member this.next =
        let cur = Seq.nth !pos tokens
        this.move
        cur
 
    member this.is_symbol token = 
        token = times || token = plus || token = open_paren || token = close_paren
 
    member this.subexpr = 
        let balanced = ref 0
        let mutable ret = []
        let mutable cur = this.next
        while !balanced <> 0 || cur <> close_paren do
            if cur = open_paren then
                balanced := !balanced + 1
            if cur = close_paren then
                balanced := !balanced - 1
 
            ret <- cur :: ret
            cur <- this.next
 
        List.rev ret
 
    member this.interpret = 
        let sk = new System.Collections.Generic.Stack<string>()
 
        while not this.is_done do
            let mutable cur = this.next
            if cur = open_paren then
                let sub = this.subexpr
                let result = (new Interpreter(sub)).evulate_expr sub
                if sk.Count = 0 || sk.Peek() <> times then
                    sk.Push(result.ToString()) |> ignore
                else 
                    this.do_multiply sk result |> ignore                    
            elif not (this.is_symbol cur) then
                if sk.Count = 0 || sk.Peek() <> times then
                    sk.Push(cur) |> ignore
                else
                    this.do_multiply sk (float cur) |> ignore
            else
                sk.Push(cur)
 
        this.do_sum(sk)
 
            
    member this.do_sum (sk : System.Collections.Generic.Stack<string>) =
        let mutable sum = 0.0
        while sk.Count <> 0 do
            let elem = sk.Pop() 
            if not (this.is_symbol elem) then
                sum <- sum + (float elem)
        sum
 
    member this.do_multiply (sk : System.Collections.Generic.Stack<string>) operand = 
        sk.Pop() |> ignore
        let operand1 = sk.Pop() |> float
        sk.Push((operand1 * operand).ToString()) |> ignore
 
    member this.evulate_expr (tokens : string seq) =
        let i = new Interpreter(tokens)
        i.interpret
 

