

type ( 't1, 't0 ) nonterm =
    {
        _Const2: ( 't1 ) Camlburg.nt;
        _Add1: ( 't0 ) Camlburg.nt;
        str: ( ( string  ) ) Camlburg.nt;
        number: ( ( int     ) ) Camlburg.nt
    }

let rec
inf =
    {number = (Camlburg.infinity)
    ;str = (Camlburg.infinity)
    ;_Add1 = (Camlburg.infinity)
    ;_Const2 = (Camlburg.infinity)
    }


let rec
update_number =
    fun nt x ->
        if nt.Camlburg.cost >= x.number.Camlburg.cost then
            x
        else
            (fun x ->
                (update_str
                    {Camlburg.cost = (nt.Camlburg.cost + 1)
                    ;Camlburg.action =
                        (fun () ->
                            let n = x.number.Camlburg.action ()
                            in
                                
# 36 "sample.nw"

                                ( string_of_int n      )
                                
# 39 "sample.ml"
)
                    })
                    x)
                { x with number = (nt) }
and update_str =
    fun nt x ->
        if nt.Camlburg.cost >= x.str.Camlburg.cost then
            x
        else
            (fun x ->
                (update_number
                    {Camlburg.cost = (nt.Camlburg.cost + 1)
                    ;Camlburg.action =
                        (fun () ->
                            let str = x.str.Camlburg.action ()
                            in
                                
# 37 "sample.nw"

                                ( int_of_string str    )
                                
# 61 "sample.ml"
)
                    })
                    x)
                { x with str = (nt) }
and update__Add1 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Add1.Camlburg.cost then
            x
        else
            { x with _Add1 = (nt) }
and update__Const2 =
    fun nt x ->
        if nt.Camlburg.cost >= x._Const2.Camlburg.cost then
            x
        else
            { x with _Const2 = (nt) }


let rec
conSub =
    fun arg1 arg2 ->
        (update_number
            {Camlburg.cost =
                (arg1.number.Camlburg.cost + arg2.number.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1.number.Camlburg.action ()
                    and m = arg2.number.Camlburg.action ()
                    in
                        
# 24 "sample.nw"
 ( n-m ) 
# 94 "sample.ml"
)
            })
            inf
and conStr =
    fun arg1 ->
        (update_str
            {Camlburg.cost =
                (let x = arg1
                in
                    
# 31 "sample.nw"

                    ( String.length x )
                    
# 109 "sample.ml"
)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1
                    in
                        
# 31 "sample.nw"
 ( x ) 
# 118 "sample.ml"
)
            })
            inf
and conMul =
    fun arg1 arg2 ->
        (update_number
            {Camlburg.cost =
                (arg1.number.Camlburg.cost + arg2.number.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1.number.Camlburg.action ()
                    and m = arg2.number.Camlburg.action ()
                    in
                        
# 25 "sample.nw"
 ( n*m ) 
# 135 "sample.ml"
)
            })
            inf
and conDiv =
    fun arg1 arg2 ->
        (update_number
            {Camlburg.cost =
                (arg1.number.Camlburg.cost + arg2.number.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let n = arg1.number.Camlburg.action ()
                    and m = arg2.number.Camlburg.action ()
                    in
                        
# 26 "sample.nw"

                        ( if m = 0 then assert false else n/m )
                        
# 154 "sample.ml"
)
            })
            inf
and conConst =
    fun arg1 ->
        (update__Const2
            {Camlburg.cost = ((Camlburg.matches 0) arg1)
            ;Camlburg.action = (fun () -> ())
            })
            ((update_number
                (Camlburg.choice
                    [{Camlburg.cost = (1)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1
                            in
                                
# 27 "sample.nw"

                                ( x )
                                
# 176 "sample.ml"
)
                    }
                    ;{Camlburg.cost = ((Camlburg.matches 0) arg1)
                    ;Camlburg.action =
                        (fun () ->
                            
# 28 "sample.nw"
 ( 0 ) 
# 185 "sample.ml"
)
                    }]))
                inf)
and conCons =
    fun arg1 arg2 ->
        (update_str
            {Camlburg.cost = (2)
            ;Camlburg.action =
                (fun () ->
                    let x = arg1
                    and y = arg2
                    in
                        
# 32 "sample.nw"
 ( x ^ y ) 
# 201 "sample.ml"
)
            })
            inf
and conAdd =
    fun arg1 arg2 ->
        (update__Add1
            {Camlburg.cost =
                (arg1.number.Camlburg.cost + arg2.number.Camlburg.cost)
            ;Camlburg.action =
                (fun () ->
                    let y = arg1.number.Camlburg.action ()
                    and z = arg2.number.Camlburg.action ()
                    in
                        (y ,z))
            })
            ((update_number
                (Camlburg.choice
                    [{Camlburg.cost =
                        (2 + arg1.number.Camlburg.cost
                        +
                        arg2.number.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.number.Camlburg.action ()
                            and y = arg2.number.Camlburg.action ()
                            in
                                
# 20 "sample.nw"

                                ( x + y )
                                
# 233 "sample.ml"
)
                    }
                    ;{Camlburg.cost =
                        (2 + arg1.number.Camlburg.cost
                        +
                        arg2._Add1.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.number.Camlburg.action ()
                            and _v1 = arg2._Add1.Camlburg.action ()
                            in
                                let (y, z) = _v1
                                in
                                    
# 21 "sample.nw"

                                    ( x + y + z)
                                    
# 252 "sample.ml"
)
                    }
                    ;{Camlburg.cost =
                        (1 + arg1.number.Camlburg.cost
                        +
                        arg2._Const2.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let x = arg1.number.Camlburg.action ()
                            and _v1 = arg2._Const2.Camlburg.action ()
                            in
                                let () = _v1
                                in
                                    
# 22 "sample.nw"

                                    ( x     )
                                    
# 271 "sample.ml"
)
                    }
                    ;{Camlburg.cost =
                        (arg1._Const2.Camlburg.cost
                        +
                        arg2._Const2.Camlburg.cost)
                    ;Camlburg.action =
                        (fun () ->
                            let _v1 = arg1._Const2.Camlburg.action ()
                            and _v2 = arg2._Const2.Camlburg.action ()
                            in
                                let () = _v2
                                in
                                    let () = _v1
                                    in
                                        
# 23 "sample.nw"

                                        ( 0     )
                                        
# 292 "sample.ml"
)
                    }]))
                inf)


