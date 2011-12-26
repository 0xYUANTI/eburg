
(* this is the header *)
(***** debug info *****
nonterm 0 : reg
nonterm 1 : sreg
nonterm 2 : ureg

rule 1 : reg:	INT	= reg_INT (1);
rule 2 : sreg:	INT	= sreg_INT (1);
rule 3 : ureg:	INT	= ureg_INT (1);
rule 4 : reg:	VAR	= reg_VAR (1);
rule 5 : reg:	sreg	= reg_sreg (1);
rule 6 : sreg:	reg	= sreg_reg (1);
rule 7 : sreg:	ureg	= sreg_ureg (1);
rule 8 : ureg:	sreg	= ureg_sreg_or_reg (1);
rule 9 : ureg:	reg	= ureg_sreg_or_reg (1);
rule 10 : reg:	ADD(reg,sreg)	= r_ADD_r_r (1);
rule 11 : reg:	ADD(sreg,reg)	= r_ADD_r_r (1);
rule 12 : reg:	ADD(INT,reg)	= r_ADD_2i_r (1);
rule 13 : reg:	ADD(reg,INT)	= r_ADD_r_2i (1);
rule 14 : reg:	ADD(INT,sreg)	= r_ADD_2ip_r (1);
rule 15 : reg:	ADD(sreg,INT)	= r_ADD_r_2ip (1);
rule 16 : sreg:	ADD(sreg,sreg)	= r_ADD_r_r (1);
rule 17 : sreg:	ADD(INT,sreg)	= r_ADD_2i_r (1);
rule 18 : sreg:	ADD(sreg,INT)	= r_ADD_r_2i (1);
rule 19 : sreg:	ADD(INT,reg)	= r_ADD_2im_r (1);
rule 20 : sreg:	ADD(reg,INT)	= r_ADD_r_2im (1);
rule 21 : reg:	SUB(reg,sreg)	= r_SUB_r_r (1);
rule 22 : reg:	SUB(INT,reg)	= r_SUB_2ipp_r (2);
rule 23 : reg:	SUB(reg,INT)	= r_SUB_r_2i (1);
rule 24 : reg:	SUB(sreg,INT)	= r_SUB_r_2im (1);
rule 25 : sreg:	SUB(sreg,sreg)	= r_SUB_r_r (1);
rule 26 : sreg:	SUB(reg,reg)	= r_SUB_r_r (1);
rule 27 : sreg:	SUB(sreg,INT)	= r_SUB_r_2i (1);
rule 28 : sreg:	SUB(reg,INT)	= r_SUB_r_2ip (1);
rule 29 : sreg:	MUL(sreg,ureg)	= r_MUL_r_r (1);
rule 30 : sreg:	MUL(ureg,sreg)	= r_MUL_r_r (1);
rule 31 : sreg:	MUL(INT,sreg)	= r_MUL_i_r (1);
rule 32 : sreg:	MUL(sreg,INT)	= r_MUL_r_i (1);
rule 33 : sreg:	MUL(INT,ureg)	= r_MUL_2i_r (1);
rule 34 : sreg:	MUL(ureg,INT)	= r_MUL_r_2i (1);
rule 35 : ureg:	DIV(sreg,sreg)	= r_DIV_r_r (1);
rule 36 : ureg:	DIV(ureg,ureg)	= r_DIV_r_r (1);
rule 37 : ureg:	DIV(ureg,INT)	= r_DIV_r_i (1);
rule 38 : reg:	NEG(reg)	= r_NEG_r_p_2 (2);
rule 39 : ureg:	NEG(ureg)	= r_NEG_r (1);
rule 40 : sreg:	NEG(sreg)	= r_NEG_r (1);
**********************)


structure BurmOps = struct
  datatype ops = T_INT
	       | T_VAR
	       | T_ADD
	       | T_SUB
	       | T_MUL
	       | T_DIV
	       | T_NEG
end


signature BURM_INPUT_SPEC = sig
  type tree
  val opchildren : tree -> BurmOps.ops * (tree list)
end


signature BURM = sig
  exception NoMatch
  type tree
  datatype rule = R_reg_INT
		| R_sreg_INT
		| R_ureg_INT
		| R_reg_VAR
		| R_reg_sreg of (rule * tree)
		| R_sreg_reg of (rule * tree)
		| R_sreg_ureg of (rule * tree)
		| R_ureg_sreg_or_reg of (rule * tree)
		| R_r_ADD_r_r of (rule * tree) * (rule * tree)
		| R_r_ADD_2i_r of (rule * tree)
		| R_r_ADD_r_2i of (rule * tree)
		| R_r_ADD_2ip_r of (rule * tree)
		| R_r_ADD_r_2ip of (rule * tree)
		| R_r_ADD_2im_r of (rule * tree)
		| R_r_ADD_r_2im of (rule * tree)
		| R_r_SUB_r_r of (rule * tree) * (rule * tree)
		| R_r_SUB_2ipp_r of (rule * tree)
		| R_r_SUB_r_2i of (rule * tree)
		| R_r_SUB_r_2im of (rule * tree)
		| R_r_SUB_r_2ip of (rule * tree)
		| R_r_MUL_r_r of (rule * tree) * (rule * tree)
		| R_r_MUL_i_r of (rule * tree)
		| R_r_MUL_r_i of (rule * tree)
		| R_r_MUL_2i_r of (rule * tree)
		| R_r_MUL_r_2i of (rule * tree)
		| R_r_DIV_r_r of (rule * tree) * (rule * tree)
		| R_r_DIV_r_i of (rule * tree)
		| R_r_NEG_r_p_2 of (rule * tree)
		| R_r_NEG_r of (rule * tree)
  val reduce : tree -> rule * tree
  val ruleToString : rule -> string
end


functor BurmGen (In : BURM_INPUT_SPEC) : BURM =
  struct

    type tree = In.tree

    exception NoMatch
  datatype rule = R_reg_INT
		| R_sreg_INT
		| R_ureg_INT
		| R_reg_VAR
		| R_reg_sreg of (rule * tree)
		| R_sreg_reg of (rule * tree)
		| R_sreg_ureg of (rule * tree)
		| R_ureg_sreg_or_reg of (rule * tree)
		| R_r_ADD_r_r of (rule * tree) * (rule * tree)
		| R_r_ADD_2i_r of (rule * tree)
		| R_r_ADD_r_2i of (rule * tree)
		| R_r_ADD_2ip_r of (rule * tree)
		| R_r_ADD_r_2ip of (rule * tree)
		| R_r_ADD_2im_r of (rule * tree)
		| R_r_ADD_r_2im of (rule * tree)
		| R_r_SUB_r_r of (rule * tree) * (rule * tree)
		| R_r_SUB_2ipp_r of (rule * tree)
		| R_r_SUB_r_2i of (rule * tree)
		| R_r_SUB_r_2im of (rule * tree)
		| R_r_SUB_r_2ip of (rule * tree)
		| R_r_MUL_r_r of (rule * tree) * (rule * tree)
		| R_r_MUL_i_r of (rule * tree)
		| R_r_MUL_r_i of (rule * tree)
		| R_r_MUL_2i_r of (rule * tree)
		| R_r_MUL_r_2i of (rule * tree)
		| R_r_DIV_r_r of (rule * tree) * (rule * tree)
		| R_r_DIV_r_i of (rule * tree)
		| R_r_NEG_r_p_2 of (rule * tree)
		| R_r_NEG_r of (rule * tree)


    fun ruleToString (R_reg_INT) = "R_reg_INT"
      | ruleToString(R_sreg_INT) = "R_sreg_INT"
      | ruleToString(R_ureg_INT) = "R_ureg_INT"
      | ruleToString(R_reg_VAR) = "R_reg_VAR"
      | ruleToString(R_reg_sreg _) = "R_reg_sreg"
      | ruleToString(R_sreg_reg _) = "R_sreg_reg"
      | ruleToString(R_sreg_ureg _) = "R_sreg_ureg"
      | ruleToString(R_ureg_sreg_or_reg _) = "R_ureg_sreg_or_reg"
      | ruleToString(R_r_ADD_r_r _) = "R_r_ADD_r_r"
      | ruleToString(R_r_ADD_2i_r _) = "R_r_ADD_2i_r"
      | ruleToString(R_r_ADD_r_2i _) = "R_r_ADD_r_2i"
      | ruleToString(R_r_ADD_2ip_r _) = "R_r_ADD_2ip_r"
      | ruleToString(R_r_ADD_r_2ip _) = "R_r_ADD_r_2ip"
      | ruleToString(R_r_ADD_2im_r _) = "R_r_ADD_2im_r"
      | ruleToString(R_r_ADD_r_2im _) = "R_r_ADD_r_2im"
      | ruleToString(R_r_SUB_r_r _) = "R_r_SUB_r_r"
      | ruleToString(R_r_SUB_2ipp_r _) = "R_r_SUB_2ipp_r"
      | ruleToString(R_r_SUB_r_2i _) = "R_r_SUB_r_2i"
      | ruleToString(R_r_SUB_r_2im _) = "R_r_SUB_r_2im"
      | ruleToString(R_r_SUB_r_2ip _) = "R_r_SUB_r_2ip"
      | ruleToString(R_r_MUL_r_r _) = "R_r_MUL_r_r"
      | ruleToString(R_r_MUL_i_r _) = "R_r_MUL_i_r"
      | ruleToString(R_r_MUL_r_i _) = "R_r_MUL_r_i"
      | ruleToString(R_r_MUL_2i_r _) = "R_r_MUL_2i_r"
      | ruleToString(R_r_MUL_r_2i _) = "R_r_MUL_r_2i"
      | ruleToString(R_r_DIV_r_r _) = "R_r_DIV_r_r"
      | ruleToString(R_r_DIV_r_i _) = "R_r_DIV_r_i"
      | ruleToString(R_r_NEG_r_p_2 _) = "R_r_NEG_r_p_2"
      | ruleToString(R_r_NEG_r _) = "R_r_NEG_r"


    type s_cost = int Array.array
    type s_rule = int Array.array
    datatype s_node =
      N_INT
    | N_VAR
    | N_ADD		of s_tree * s_tree
    | N_SUB		of s_tree * s_tree
    | N_MUL		of s_tree * s_tree
    | N_DIV		of s_tree * s_tree
    | N_NEG		of s_tree
    withtype s_tree = s_cost * s_rule * s_node * tree


    val sub = Array.sub
    val update = Array.update
    val leaf_N_INT =
      (Array.fromList [1,1,1],
       Array.fromList [1,2,3],
       N_INT)
    val leaf_N_VAR =
      (Array.fromList [1,2,2],
       Array.fromList [4,6,9],
       N_VAR)
    val s_c_nothing = Array.array (3,16383)
    val s_r_nothing = Array.array (3,0)


    fun rec_label (tree : In.tree) =
      let
	fun closure_reg (s_c, s_r, c) =
	  (if c + 1 < sub (s_c,2) then
	     (update (s_c,2,c + 1);
	      update (s_r,2,9);
	      closure_ureg (s_c, s_r, c + 1)
	     )
	   else
	     ();
	   if c + 1 < sub (s_c,1) then
	     (update (s_c,1,c + 1);
	      update (s_r,1,6);
	      closure_sreg (s_c, s_r, c + 1)
	     )
	   else
	     ()
	  )
	and closure_sreg (s_c, s_r, c) =
	  (if c + 1 < sub (s_c,2) then
	     (update (s_c,2,c + 1);
	      update (s_r,2,8);
	      closure_ureg (s_c, s_r, c + 1)
	     )
	   else
	     ();
	   if c + 1 < sub (s_c,0) then
	     (update (s_c,0,c + 1);
	      update (s_r,0,5);
	      closure_reg (s_c, s_r, c + 1)
	     )
	   else
	     ()
	  )
	and closure_ureg (s_c, s_r, c) =
	  (if c + 1 < sub (s_c,1) then
	     (update (s_c,1,c + 1);
	      update (s_r,1,7);
	      closure_sreg (s_c, s_r, c + 1)
	     )
	   else
	     ()
	  )
	val (term, children) = In.opchildren tree
	val (s_c, s_r, t) = case term of
	  BurmOps.T_INT =>
	    leaf_N_INT
	| BurmOps.T_VAR =>
	    leaf_N_VAR
	| BurmOps.T_ADD =>
	    let
	      val [t0,t1] = map rec_label children
	    in
	      let val (s_c, s_r) = case (t0,t1) of
		    z =>
		let
		  val s_c = Array.array (3,16383)
		  val s_r = Array.array (3,0)
		in
		case z of
		    ((s0_c,s0_r,_,_),(_,_,N_INT,_)) =>
		      (
		       if sub (s0_r,1)<>0 then
			 let
			   val c = sub (s0_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 18);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 15);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,0)<>0 then
			 let
			   val c = sub (s0_c,0)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 20);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 13);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  | _ => ()
		  ;
		case z of
		    ((_,_,N_INT,_),(s0_c,s0_r,_,_)) =>
		      (
		       if sub (s0_r,1)<>0 then
			 let
			   val c = sub (s0_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 17);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 14);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,0)<>0 then
			 let
			   val c = sub (s0_c,0)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 19);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 12);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  | _ => ()
		  ;
		case z of
		    ((s0_c,s0_r,_,_),(s1_c,s1_r,_,_)) =>
		      (
		       if sub (s0_r,1)<>0 andalso sub (s1_r,1)<>0 then
			 let
			   val c = sub (s0_c,1) + sub (s1_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 16);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,1)<>0 andalso sub (s1_r,0)<>0 then
			 let
			   val c = sub (s0_c,1) + sub (s1_c,0)
			 in
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 11);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,0)<>0 andalso sub (s1_r,1)<>0 then
			 let
			   val c = sub (s0_c,0) + sub (s1_c,1)
			 in
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 10);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  ;
		  (s_c, s_r)
		end
	      in (s_c, s_r, N_ADD (t0,t1)) end
	    end
	| BurmOps.T_SUB =>
	    let
	      val [t0,t1] = map rec_label children
	    in
	      let val (s_c, s_r) = case (t0,t1) of
		    z =>
		let
		  val s_c = Array.array (3,16383)
		  val s_r = Array.array (3,0)
		in
		case z of
		    ((s0_c,s0_r,_,_),(_,_,N_INT,_)) =>
		      (
		       if sub (s0_r,1)<>0 then
			 let
			   val c = sub (s0_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 27);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 24);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,0)<>0 then
			 let
			   val c = sub (s0_c,0)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 28);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 23);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  | _ => ()
		  ;
		case z of
		    ((s0_c,s0_r,_,_),(s1_c,s1_r,_,_)) =>
		      (
		       if sub (s0_r,0)<>0 andalso sub (s1_r,0)<>0 then
			 let
			   val c = sub (s0_c,0) + sub (s1_c,0)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 26);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,1)<>0 andalso sub (s1_r,1)<>0 then
			 let
			   val c = sub (s0_c,1) + sub (s1_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 25);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,0)<>0 andalso sub (s1_r,1)<>0 then
			 let
			   val c = sub (s0_c,0) + sub (s1_c,1)
			 in
			   if c + 1 < sub (s_c,0) then
			     (update (s_c, 0, c + 1);
			      update (s_r, 0, 21);
			      closure_reg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  ;
		case z of
		    ((_,_,N_INT,_),(s0_c,s0_r,_,_)) =>
		      (
		       if sub (s0_r,0)<>0 then
			 let
			   val c = sub (s0_c,0)
			 in
			   if c + 2 < sub (s_c,0) then
			     (update (s_c, 0, c + 2);
			      update (s_r, 0, 22);
			      closure_reg (s_c, s_r, c + 2);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  | _ => ()
		  ;
		  (s_c, s_r)
		end
	      in (s_c, s_r, N_SUB (t0,t1)) end
	    end
	| BurmOps.T_MUL =>
	    let
	      val [t0,t1] = map rec_label children
	    in
	      let val (s_c, s_r) = case (t0,t1) of
		    z =>
		let
		  val s_c = Array.array (3,16383)
		  val s_r = Array.array (3,0)
		in
		case z of
		    ((s0_c,s0_r,_,_),(_,_,N_INT,_)) =>
		      (
		       if sub (s0_r,2)<>0 then
			 let
			   val c = sub (s0_c,2)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 34);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,1)<>0 then
			 let
			   val c = sub (s0_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 32);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  | _ => ()
		  ;
		case z of
		    ((_,_,N_INT,_),(s0_c,s0_r,_,_)) =>
		      (
		       if sub (s0_r,2)<>0 then
			 let
			   val c = sub (s0_c,2)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 33);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,1)<>0 then
			 let
			   val c = sub (s0_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 31);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  | _ => ()
		  ;
		case z of
		    ((s0_c,s0_r,_,_),(s1_c,s1_r,_,_)) =>
		      (
		       if sub (s0_r,2)<>0 andalso sub (s1_r,1)<>0 then
			 let
			   val c = sub (s0_c,2) + sub (s1_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 30);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,1)<>0 andalso sub (s1_r,2)<>0 then
			 let
			   val c = sub (s0_c,1) + sub (s1_c,2)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 29);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  ;
		  (s_c, s_r)
		end
	      in (s_c, s_r, N_MUL (t0,t1)) end
	    end
	| BurmOps.T_DIV =>
	    let
	      val [t0,t1] = map rec_label children
	    in
	      let val (s_c, s_r) = case (t0,t1) of
		    z =>
		let
		  val s_c = Array.array (3,16383)
		  val s_r = Array.array (3,0)
		in
		case z of
		    ((s0_c,s0_r,_,_),(_,_,N_INT,_)) =>
		      (
		       if sub (s0_r,2)<>0 then
			 let
			   val c = sub (s0_c,2)
			 in
			   if c + 1 < sub (s_c,2) then
			     (update (s_c, 2, c + 1);
			      update (s_r, 2, 37);
			      closure_ureg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  | _ => ()
		  ;
		case z of
		    ((s0_c,s0_r,_,_),(s1_c,s1_r,_,_)) =>
		      (
		       if sub (s0_r,2)<>0 andalso sub (s1_r,2)<>0 then
			 let
			   val c = sub (s0_c,2) + sub (s1_c,2)
			 in
			   if c + 1 < sub (s_c,2) then
			     (update (s_c, 2, c + 1);
			      update (s_r, 2, 36);
			      closure_ureg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,1)<>0 andalso sub (s1_r,1)<>0 then
			 let
			   val c = sub (s0_c,1) + sub (s1_c,1)
			 in
			   if c + 1 < sub (s_c,2) then
			     (update (s_c, 2, c + 1);
			      update (s_r, 2, 35);
			      closure_ureg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  ;
		  (s_c, s_r)
		end
	      in (s_c, s_r, N_DIV (t0,t1)) end
	    end
	| BurmOps.T_NEG =>
	    let
	      val [t0] = map rec_label children
	    in
	      let val (s_c, s_r) = case (t0) of
		    z =>
		let
		  val s_c = Array.array (3,16383)
		  val s_r = Array.array (3,0)
		in
		case z of
		    ((s0_c,s0_r,_,_)) =>
		      (
		       if sub (s0_r,1)<>0 then
			 let
			   val c = sub (s0_c,1)
			 in
			   if c + 1 < sub (s_c,1) then
			     (update (s_c, 1, c + 1);
			      update (s_r, 1, 40);
			      closure_sreg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,2)<>0 then
			 let
			   val c = sub (s0_c,2)
			 in
			   if c + 1 < sub (s_c,2) then
			     (update (s_c, 2, c + 1);
			      update (s_r, 2, 39);
			      closure_ureg (s_c, s_r, c + 1);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       if sub (s0_r,0)<>0 then
			 let
			   val c = sub (s0_c,0)
			 in
			   if c + 2 < sub (s_c,0) then
			     (update (s_c, 0, c + 2);
			      update (s_r, 0, 38);
			      closure_reg (s_c, s_r, c + 2);
			     ())
			   else ();
			   ()
			 end
		       else ();
		       ()
		      )
		  ;
		  (s_c, s_r)
		end
	      in (s_c, s_r, N_NEG (t0)) end
	    end
      in
        (s_c, s_r, t, tree)
      end

    fun doreduce (stree : s_tree, nt) =
      let
	val (s_c, s_r, _, tree) = stree
	val cost = sub (s_c, nt)
      in
	if cost=16383 then
	  (print ("No Match on nonterminal "^(Int.toString nt)^"\n");
	   print "Possibilities were :\n";
	   let
	     fun loop n =
	       let
	         val c = Array.sub (s_c, n);
	         val r = Array.sub (s_r, n);
	       in
	         if c=16383 then () else
	           print ("rule "^(Int.toString r)^" with cost "
	                  ^(Int.toString c)^"\n");
	         loop (n+1)
	       end
	   in
	     (loop 0) handle General.Subscript => ()
	   end;
	   raise NoMatch)
	else
	  let
	    val rulensons =
	      case (sub (s_r, nt), stree) of
		(1, (_,_,N_INT,_)) =>
		  (R_reg_INT)
	      | (2, (_,_,N_INT,_)) =>
		  (R_sreg_INT)
	      | (3, (_,_,N_INT,_)) =>
		  (R_ureg_INT)
	      | (4, (_,_,N_VAR,_)) =>
		  (R_reg_VAR)
	      | (5, t0) =>
		  (R_reg_sreg (doreduce (t0,1)))
	      | (6, t0) =>
		  (R_sreg_reg (doreduce (t0,0)))
	      | (7, t0) =>
		  (R_sreg_ureg (doreduce (t0,2)))
	      | (8, t0) =>
		  (R_ureg_sreg_or_reg (doreduce (t0,1)))
	      | (9, t0) =>
		  (R_ureg_sreg_or_reg (doreduce (t0,0)))
	      | (10, (_,_,N_ADD (t0,t1),_)) =>
		  (R_r_ADD_r_r (doreduce (t0,0), doreduce (t1,1)))
	      | (11, (_,_,N_ADD (t0,t1),_)) =>
		  (R_r_ADD_r_r (doreduce (t0,1), doreduce (t1,0)))
	      | (12, (_,_,N_ADD ((_,_,N_INT,_),t0),_)) =>
		  (R_r_ADD_2i_r (doreduce (t0,0)))
	      | (13, (_,_,N_ADD (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_ADD_r_2i (doreduce (t0,0)))
	      | (14, (_,_,N_ADD ((_,_,N_INT,_),t0),_)) =>
		  (R_r_ADD_2ip_r (doreduce (t0,1)))
	      | (15, (_,_,N_ADD (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_ADD_r_2ip (doreduce (t0,1)))
	      | (16, (_,_,N_ADD (t0,t1),_)) =>
		  (R_r_ADD_r_r (doreduce (t0,1), doreduce (t1,1)))
	      | (17, (_,_,N_ADD ((_,_,N_INT,_),t0),_)) =>
		  (R_r_ADD_2i_r (doreduce (t0,1)))
	      | (18, (_,_,N_ADD (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_ADD_r_2i (doreduce (t0,1)))
	      | (19, (_,_,N_ADD ((_,_,N_INT,_),t0),_)) =>
		  (R_r_ADD_2im_r (doreduce (t0,0)))
	      | (20, (_,_,N_ADD (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_ADD_r_2im (doreduce (t0,0)))
	      | (21, (_,_,N_SUB (t0,t1),_)) =>
		  (R_r_SUB_r_r (doreduce (t0,0), doreduce (t1,1)))
	      | (22, (_,_,N_SUB ((_,_,N_INT,_),t0),_)) =>
		  (R_r_SUB_2ipp_r (doreduce (t0,0)))
	      | (23, (_,_,N_SUB (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_SUB_r_2i (doreduce (t0,0)))
	      | (24, (_,_,N_SUB (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_SUB_r_2im (doreduce (t0,1)))
	      | (25, (_,_,N_SUB (t0,t1),_)) =>
		  (R_r_SUB_r_r (doreduce (t0,1), doreduce (t1,1)))
	      | (26, (_,_,N_SUB (t0,t1),_)) =>
		  (R_r_SUB_r_r (doreduce (t0,0), doreduce (t1,0)))
	      | (27, (_,_,N_SUB (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_SUB_r_2i (doreduce (t0,1)))
	      | (28, (_,_,N_SUB (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_SUB_r_2ip (doreduce (t0,0)))
	      | (29, (_,_,N_MUL (t0,t1),_)) =>
		  (R_r_MUL_r_r (doreduce (t0,1), doreduce (t1,2)))
	      | (30, (_,_,N_MUL (t0,t1),_)) =>
		  (R_r_MUL_r_r (doreduce (t0,2), doreduce (t1,1)))
	      | (31, (_,_,N_MUL ((_,_,N_INT,_),t0),_)) =>
		  (R_r_MUL_i_r (doreduce (t0,1)))
	      | (32, (_,_,N_MUL (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_MUL_r_i (doreduce (t0,1)))
	      | (33, (_,_,N_MUL ((_,_,N_INT,_),t0),_)) =>
		  (R_r_MUL_2i_r (doreduce (t0,2)))
	      | (34, (_,_,N_MUL (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_MUL_r_2i (doreduce (t0,2)))
	      | (35, (_,_,N_DIV (t0,t1),_)) =>
		  (R_r_DIV_r_r (doreduce (t0,1), doreduce (t1,1)))
	      | (36, (_,_,N_DIV (t0,t1),_)) =>
		  (R_r_DIV_r_r (doreduce (t0,2), doreduce (t1,2)))
	      | (37, (_,_,N_DIV (t0,(_,_,N_INT,_)),_)) =>
		  (R_r_DIV_r_i (doreduce (t0,2)))
	      | (38, (_,_,N_NEG t0,_)) =>
		  (R_r_NEG_r_p_2 (doreduce (t0,0)))
	      | (39, (_,_,N_NEG t0,_)) =>
		  (R_r_NEG_r (doreduce (t0,2)))
	      | (40, (_,_,N_NEG t0,_)) =>
		  (R_r_NEG_r (doreduce (t0,1)))
	      | _ => raise NoMatch (* bug in iburg *)
	  in
	    (rulensons, tree)
	  end
      end

    fun reduce (tree) =
      doreduce (rec_label (tree), 0)
  end




structure In = struct

  open BurmOps

  datatype tree =
    INT of int
  | VAR of string
  | ADD of tree * tree
  | SUB of tree * tree
  | MUL of tree * tree
  | DIV of tree * tree
  | NEG of tree

  fun opchildren t =
    case t of
      INT _ =>       (T_INT, [])
    | VAR _ =>       (T_VAR, [])
    | ADD (t1,t2) => (T_ADD, [t1,t2])
    | SUB (t1,t2) => (T_SUB, [t1,t2])
    | MUL (t1,t2) => (T_MUL, [t1,t2])
    | DIV (t1,t2) => (T_DIV, [t1,t2])
    | NEG (t1) =>    (T_NEG, [t1])

end




structure Example = struct 

  structure Burm = BurmGen (In)
  open In

  fun say s = print s


  local
    val num = ref 1
    fun inc iref = iref := (!iref + 1)
  in
    fun resetreg () = (num := 1)
    fun newreg () = ("r"^(Int.toString (!num)) before inc num)
  end


  fun walk (Burm.R_reg_INT, INT n) =
        let val reg = newreg () in
	  say ("ldi "^reg^","^(Int.toString (n+n+1))^"\n"); reg
	end
    | walk (Burm.R_sreg_INT, INT n) =
	let val reg = newreg () in
	  say ("ldi "^reg^","^(Int.toString (n+n))^"\n"); reg
	end
    | walk (Burm.R_ureg_INT, INT n) =
	let val reg = newreg () in
	  say ("ldi "^reg^","^(Int.toString n)^"\n"); reg
	end
    | walk (Burm.R_reg_VAR, VAR v) =
	let val reg = newreg () in
	  say ("ld "^reg^",["^v^"]\n"); reg
	end
    | walk (Burm.R_reg_sreg reg, _) =
        let val reg' = walk reg in
	  say ("inc "^reg'^"\n"); reg'
	end
    | walk (Burm.R_sreg_reg reg, _) =
        let val reg' = walk reg in
	  say ("dec "^reg'^"\n"); reg'
	end
    | walk (Burm.R_sreg_ureg reg, _) =
        let val reg' = walk reg in
	  say ("shl "^reg'^"\n"); reg'
	end
    | walk (Burm.R_ureg_sreg_or_reg reg, _) =
        let val reg' = walk reg in
	  say ("shr "^reg'^"\n"); reg'
	end
    | walk (Burm.R_r_ADD_r_r (r1,r2), _) =
	let val (r1',r2') = (walk r1, walk r2) in
	  say ("add "^r1'^","^r2'^"\n"); r1'
	end
    | walk (Burm.R_r_ADD_2i_r reg, ADD (INT n,_)) =
        let val reg' = walk reg in
	  say ("addi "^reg'^","^(Int.toString (n+n))^"\n"); reg'
	end
    | walk (Burm.R_r_ADD_r_2i reg, ADD (_,INT n)) =
        let val reg' = walk reg in
	  say ("addi "^reg'^","^(Int.toString (n+n))^"\n"); reg'
	end
    | walk (Burm.R_r_ADD_2ip_r reg, ADD (INT n,_)) =
        let val reg' = walk reg in
	  say ("addi "^reg'^","^(Int.toString (n+n+1))^"\n"); reg'
	end
    | walk (Burm.R_r_ADD_r_2ip reg, ADD (_,INT n)) =
        let val reg' = walk reg in
	  say ("addi "^reg'^","^(Int.toString (n+n+1))^"\n"); reg'
	end
    | walk (Burm.R_r_ADD_2im_r reg, ADD (INT n,_)) =
        let val reg' = walk reg in
	  say ("addi "^reg'^","^(Int.toString (n+n-1))^"\n"); reg'
	end
    | walk (Burm.R_r_ADD_r_2im reg, ADD (_,INT n)) =
        let val reg' = walk reg in
	  say ("addi "^reg'^","^(Int.toString (n+n-1))^"\n"); reg'
	end
    | walk (Burm.R_r_SUB_r_r (r1,r2), _) =
	let val (r1',r2') = (walk r1, walk r2) in
	   say ("sub "^r1'^","^r2'^"\n"); r1'
	end
    | walk (Burm.R_r_SUB_2ipp_r reg, SUB (INT n,_)) =
        let val reg' = walk reg val r = newreg () in
	   say ("ldi "^r^","^(Int.toString (n+n+1))^"\n");
	   say ("sub "^r^","^reg'^"\n");
	   r
	end
    | walk (Burm.R_r_SUB_r_2i reg, SUB (_,INT n)) =
        let val reg' = walk reg in
	  say ("subi "^reg'^","^(Int.toString (n+n))^"\n"); reg'
	end
    | walk (Burm.R_r_SUB_r_2im reg, SUB (_,INT n)) =
        let val reg' = walk reg in
	  say ("subi "^reg'^","^(Int.toString (n+n-1))^"\n"); reg'
	end
    | walk (Burm.R_r_SUB_r_2ip reg, SUB (_,INT n)) =
        let val reg' = walk reg in
	  say ("subi "^reg'^","^(Int.toString (n+n+1))^"\n"); reg'
	end
    | walk (Burm.R_r_MUL_r_r (r1,r2), _) =
	let val (r1',r2') = (walk r1, walk r2) in
	  say ("mul "^r1'^","^r2'^"\n"); r1'
	end
    | walk (Burm.R_r_MUL_i_r reg, MUL (INT n,_)) =
        let val reg' = walk reg in
	  say ("muli "^reg'^","^(Int.toString (n))^"\n"); reg'
	end
    | walk (Burm.R_r_MUL_r_i reg, MUL (_,INT n)) =
        let val reg' = walk reg in
	  say ("muli "^reg'^","^(Int.toString (n))^"\n"); reg'
	end
    | walk (Burm.R_r_MUL_2i_r reg, MUL (INT n,_)) =
        let val reg' = walk reg in
	  say ("muli "^reg'^","^(Int.toString (n+n))^"\n"); reg'
	end
    | walk (Burm.R_r_MUL_r_2i reg, MUL (_,INT n)) =
        let val reg' = walk reg in
	  say ("muli "^reg'^","^(Int.toString (n+n))^"\n"); reg'
	end
    | walk (Burm.R_r_DIV_r_r (r1,r2), _) =
	let val (r1',r2') = (walk r1, walk r2) in
	  say ("div "^r1'^","^r2'^"\n"); r1'
	end
    | walk (Burm.R_r_DIV_r_i reg, DIV (_,INT n)) =
        let val reg' = walk reg in
	  say ("divi "^reg'^","^(Int.toString (n))^"\n"); reg'
	end
    | walk (Burm.R_r_NEG_r reg, _) =
        let val reg' = walk reg in
	  say ("neg "^reg'^"\n"); reg'
	end
    | walk (Burm.R_r_NEG_r_p_2 reg, _) =
        let val reg' = walk reg in
	  say ("neg "^reg'^"\n");
	  say ("addi "^reg'^",\n");
	  reg'
	end
    | walk _ = (print "Error, bad match in walk\n"; raise Match)


  fun doit t = walk (Burm.reduce t)

  val a = SUB (ADD (VAR "a", INT 2), INT 5)
  val b = ADD (DIV (SUB (VAR "a", INT 1), VAR "b"), INT 1)
  val c = ADD (VAR "a", INT 1)

end

