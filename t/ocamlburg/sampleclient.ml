
# 50 "sample.nw"
type number =
    | Add       of number * number
    | Sub       of number * number
    | Div       of number * number
    | Mul       of number * number
    | Const     of int
    | Str       of str

and str =       
    | String    of string
    | Cons      of string * string
    | Number    of number

# 67 "sample.nw"
let add x y     = Add(x,y)
let sub x y     = Sub(x,y)
let mul x y     = Mul(x,y)
let div x y     = Div(x,y)
let const x     = Const(x)
let str s       = Str(s)

let string s    = String(s)
let cons x y    = Cons(x,y)
let number x    = Number(x)

# 83 "sample.nw"
module S = Sample

let rec fold_num = function
    | Add(l,r) -> S.conAdd (fold_num l) (fold_num r)
    | Sub(l,r) -> S.conSub (fold_num l) (fold_num r)
    | Mul(l,r) -> S.conMul (fold_num l) (fold_num r)
    | Div(l,r) -> S.conDiv (fold_num l) (fold_num r)
    | Const(x) -> S.conConst x
    | Str(s)   -> fold_str s

and fold_str = function
    | String(x) -> S.conStr x
    | Cons(x,y) -> S.conCons x y
    | Number(n) -> fold_num n

# 102 "sample.nw"
let exp0 = (* costs: number: 5, str: 6 *)
    add 
        (const 0)
        (add 
            (const 3) 
            (const 7)) 

let exp1 = (* costs: number: 10, str: 11 *)
    add 
        (add 
            (str (string "123"))
            (str (number (const 8))))
        (add 
            (const 7)
            (const 0)) 

let number_cost exp = (fold_num exp).S.number.Camlburg.cost 
let str_cost    exp = (fold_num exp).S.str.Camlburg.cost 
let to_number   exp = (fold_num exp).S.number.Camlburg.action ()
let to_string   exp = (fold_num exp).S.str.Camlburg.action ()
