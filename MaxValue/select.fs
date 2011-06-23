module Select
 
exception OperandNumberException of string
 
 
let choose2 (l : seq<_>) =    (* seq<_> could be written as 'a seq *)
    [ for i = 0 to Seq.length l - 1 do
        for j = i + 1 to Seq.length l - 1 do
            yield [Seq.nth i l; Seq.nth j l] ]
 
let choose2_tuple (l : 'a seq) =
    [ for i = 0 to Seq.length l - 1 do
        for j = i + 1 to Seq.length l - 1 do
            yield Seq.nth i l, Seq.nth j l ]
 
let rec select n (l : 'a seq) =
 
    let merge elem l = 
        [ for e in l -> elem :: e ]
 
    let rec choose n (l : 'a seq) =
        if n = 1 then
            [for elem in l -> [elem]]
        elif n = 2 then
            choose2 l
        else
            match Seq.toList l with
            [] -> []
            |hd::tl -> 
                let current = choose (n - 1) tl |> merge hd
                let rest = choose n tl
                current @ rest
    
    choose n l
 
      
let indexer (l : 'a seq) =
    [ for i = 0 to Seq.length l - 1 do
        for j = i + 1 to Seq.length l - 1 do
            yield Seq.nth i l, Seq.nth j l ]
 
let good_pair pair = 
    let first = fst pair
    let first_beg = fst first
    let first_end = snd first
 
    let second = snd pair
    let second_beg = fst second
    let second_end = snd second
 
    first_beg >= second_beg && first_end <= second_end || 
    first_beg <= second_beg && first_end >= second_end || 
    first_beg > second_end || 
    second_beg > first_end
 
    
let predicate (l : (int * int) list) =           (* l could be written as 'a list *)
    let pair = choose2_tuple l
    pair |> Seq.forall (fun x -> good_pair x)
 
    
let rec group n ret (l :int seq) =
    let group_n n (l : int seq) =
        let candidate = indexer l |> select n
        candidate |> List.filter predicate
 
    if n = 1 || n = 2 then
        let r = group_n n l
        List.append r ret
    else
        let r = group_n n l
        let c = List.append r ret
        group (n - 1) c l
    
 
let tokenize (str : string) =     
    str.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> List.ofArray 
            
 
let map shift_n (l : (int * int) seq) =
    let shift k = k * 2
 
    let open_p = l |> Seq.groupBy (fun p -> fst p)
                   |> Seq.map (fun (k, v) -> (shift k, String.replicate (Seq.length v ) Interpret.open_paren))
 
    let close_p = l |> Seq.groupBy (fun p -> snd p)
                    |> Seq.map (fun (k, v) -> (shift k, String.replicate (Seq.length v) Interpret.close_paren))
 
    Seq.append open_p close_p |> Map.ofSeq
 
let put_paren (tokens : string list) (m : (int, string) Map) =
    let mutable ret = []
 
    for i = 0 to List.length tokens - 1 do
        let v = Map.tryFind i m
        match v with
        |None -> ret <- (List.nth tokens i) :: ret
        |Some x ->
            if x.StartsWith(Interpret.open_paren) then
                let arr = Array.create (x.Length) Interpret.open_paren
                ret <- List.append (List.ofArray arr) ret
                ret <- (List.nth tokens i) :: ret
            else
                ret <- (List.nth tokens i) :: ret
                let arr2 = Array.create x.Length Interpret.close_paren
                ret <- List.append (List.ofArray arr2) ret
    List.rev ret
 
let compute input =
    let tokens = tokenize input
    let n = (System.Math.Ceiling(float tokens.Length / 2.0) |> int) - 1
    if n = 0 then
        OperandNumberException("at least one operator is required") |> raise
 
    let idx = [|0..n|]
    let ms = idx |> group n []
    let maps = [for elm in ms -> map (n - 1) elm]
    let results = [for m in maps -> put_paren tokens m]
 
    results |> Seq.map (fun x -> (new Interpret.Interpreter(x)).interpret)
 
let max input = 
    compute input |> Seq.max
 
 (* the to_string function is for debug/display purpose, it will display all the expression with parenthese in right places *)

let to_string input =
 
    let put_paren_in_string (tokens : string list) (m : Map<int, string>) = 
        let buf = new System.Text.StringBuilder()
    
        for i = 0 to List.length tokens - 1 do
            let v = Map.tryFind i m
 
            match v with
            |None -> buf.Append(List.nth tokens i) |> ignore
            |Some x -> 
                if x.StartsWith (Interpret.open_paren) then
                    buf.Append(x).Append(List.nth tokens i) |> ignore
                else
                    buf.Append(List.nth tokens i).Append(x) |> ignore
 
        buf.ToString()
 
    let tokens = tokenize input
    let n = (System.Math.Ceiling(float tokens.Length / 2.0) |> int) - 1
    let idx = [|0..n|]
    let ms = idx |> group n []
    let maps = [for elm in ms -> map (n - 1) elm]
    [for m in maps -> put_paren_in_string tokens m]
 
