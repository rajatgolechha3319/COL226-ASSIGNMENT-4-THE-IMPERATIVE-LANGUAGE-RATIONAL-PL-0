structure DataTypes =
struct

datatype AST = PROG of BLK
and BLK = BLK of (DEC list) * (CMD list)
and DEC = VDEC of ( Type * (string list) ) | PDEC of PDEF list 
and PDEF = PDEF of Type2 * string * BLK
and Type = Int | Bool | Rational 
and Type2 = procedure
and CMD = SET of string * Exp | Call of string | Read of string | Print of Exp | ITE of Exp * ( CMD list ) * ( CMD list ) | WH of Exp * ( CMD list )
and Exp = REXP of REXP | IEXP of IEXP | BEXP of BEXP | EXP of Exp | EQEXP of Exp * Exp | NEQEX of Exp * Exp
and REXP = NEGR of REXP 
        | INVERSE of REXP
        | ADDRAT of REXP * REXP
        | SUBRAT of REXP * REXP
        | MULRAT of REXP * REXP
        | DIVRAT of REXP * REXP
        | MAKERAT of IEXP * IEXP
        | RAT of IEXP
        | SHOWRAT of REXP
        | SHOWDECIMAL of REXP
        | FROMDECIMAL of REXP
        | TODECIMAL of REXP
        | RNUM of string
        | IDR of string
        | EQR of REXP * REXP
        | NEQR of REXP * REXP
        | GTR of REXP * REXP
        | GTER of REXP * REXP
        | LTR of REXP * REXP
        | LTER of REXP * REXP
and IEXP = NEG of IEXP
        | ADD of IEXP * IEXP
        | SUB of IEXP * IEXP
        | MUL of IEXP * IEXP 
        | DIV of IEXP * IEXP
        | MOD of IEXP * IEXP
        | EQ  of IEXP * IEXP
        | NEQ of IEXP * IEXP
        | GT  of IEXP * IEXP
        | GTE of IEXP * IEXP
        | LT  of IEXP * IEXP
        | LTE of IEXP * IEXP
        | ID of string
        | POSNUM of string
and BEXP = NOT of BEXP
        | AND of BEXP * BEXP
        | OR of BEXP * BEXP
        | EQB of BEXP * BEXP
        | NEQB of BEXP * BEXP
        | TTT of string
        | FFF of string
        | IDB of string

fun strip(x:string)=
        let
                val stripper= String.concat (String.tokens (fn c => c = #"\n") x);
        in
                stripper
        end
fun length nil = 0 | length (x::xs) = 1 + (length xs); (*Function to calculate
  the length of a given list*)

datatype dict = Dict of (string * (Type * string)) list
exception NotFoundTheElement
exception Redeclaration
exception TypeMisMatch

fun copyDict (Dict lst) =
  let
    fun copyPair (key, (t, value)) = (key, (t, value))
  in
    Dict (List.map copyPair lst)
  end

fun bogus()=132
fun error()= raise Redeclaration
fun error2()= raise TypeMisMatch


fun printDictList (dictList: dict ref list ref) =
  let
    fun printDict (dict: dict ref) =
      let
        val Dict lst = !dict
        fun printPair (key, (typ, value)) = 
          print (key ^ " : "  ^ " : " ^ value ^ "\n")
      in
        List.app printPair lst
      end
  in
    List.app printDict (!dictList)
  end

fun printDict2 (dict: dict ref) =
      let
        val Dict lst = !dict
        fun printPair (key, (typ, value)) = 
          print (key ^ " : "  ^ " : " ^ value ^ "\n")
      in
        List.app printPair lst
      end

fun find_id (Dict lst, id) = 
  case List.find (fn (x, _) => x = id) lst of
    SOME (_, value) => value
  | NONE => raise NotFoundTheElement

fun update_id (Dict lst, name, intboolrat , value) =
  let
    val updatedList = List.map (fn (x, (t, v)) => if x = name andalso t = intboolrat then (x, (t, value)) else (x, (t, v))) lst
  in
    Dict updatedList
  end;

fun exist_id (Dict lst, id) = 
  case List.find (fn (x, _) => x = id) lst of
    SOME (_, value) => 1
  | NONE => 0

fun add_id (Dict lst, id ,t: Type, str: string) = 
  if List.exists (fn (x, _) => x = id) lst then
    update_id(Dict lst,id,t,str)
  else
    Dict ((id, (t, str))::lst)


fun get_value (Dict lst, name)=
        let
                val (t, str) = find_id (Dict lst, name);
        in
                str
        end;

fun get_type (Dict lst, name)=
        let
                val (ty, str) = find_id (Dict lst, name);
        in
                ty
        end;

fun get_proceed ( lst,name)=
  case List.find (fn (x, _) => x = name) lst of
    SOME (_, value) => value
  | NONE => raise NotFoundTheElement

val stack = ref ( [ ref (Dict [])] );
val globaldict =hd(!stack);
val procedlist = ref ( [] : (string * BLK) list);
val output = ref( [] : string list);

fun addvars(ty: Type ,ids: string list)=
        if length(ids)=0 then 1
        else
                if exist_id(!globaldict,hd(ids))=1 then 
                                                let
                                                        val new_dict = update_id(!globaldict,hd(ids),ty,"NULL")
                                                in
                                                        globaldict := new_dict;
                                                        addvars(ty,tl(ids))
                                                end
                else
                        let
                                val new_dict = add_id(!globaldict,hd(ids),ty,"NULL")
                        in
                                globaldict := new_dict;
                                addvars(ty,tl(ids))
                        end
        
fun procedupdate(x : PDEF list)=
        if length(x)=0 then 1
        else
                let 
                        val curr_proced=hd(x);
                        val PDEF(a,b,c)=hd(x);
                in
                        procedlist := [(b,c)] @ !procedlist;
                        procedupdate(tl(x))
                end


fun dechandle(x: DEC list)=
        if length(x)=0 then 1
        else 
                let 
                        val decl = hd(x)
                in
                        case decl of
                        PDEC(pdefs)=>
                                        let
                                                val proc_upd = procedupdate(pdefs)
                                        in
                                                1
                                        end
                        |VDEC(ty,ids)=>
                                        let
                                                val new_dict = addvars(ty,ids)
                                        in
                                                dechandle(tl(x))
                                        end
                        
                end

fun y(xi: bool)=
        if xi then "true" else "false"

fun y2(xi: bool)=
        if xi then "false" else "true"
fun ynot(xi: string)=
        if xi="true" then "false" else "true"

fun yand(xi: string, xi2: string)=
        if xi="true" andalso xi2="true" then "true" else "false"
fun yor(xi: string , xi2: string)=
        if xi="true" orelse xi2="true" then "true" else "false"
fun yeq(x1: string,x2: string)=
        if x1="true" andalso x2="true" then "true"
        else if x1="false" andalso x2="false" then "true"
        else "false"

fun intex(x)=
        let
        open BigInt
        in
        case x of 
        NEG(ex)=>       BigInt.toString(BigInt.neg(valOf(BigInt.fromString(intex(ex)))))
        |ADD(ex1,ex2)=> BigInt.toString(BigInt.add(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |SUB(ex1,ex2)=> BigInt.toString(BigInt.sub(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |MUL(ex1,ex2)=> BigInt.toString(BigInt.mul(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |DIV(ex1,ex2)=> BigInt.toString(BigInt.divb(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |MOD(ex1,ex2)=> BigInt.toString(BigInt.modb(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |EQ(ex1,ex2) => y(BigInt.eq(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |NEQ(ex1,ex2)=>  y2((BigInt.eq(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2))))))
        |LT(ex1,ex2)=> y(BigInt.lt(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |LTE(ex1,ex2)=> y(BigInt.le(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |GT(ex1,ex2)=> y(BigInt.gt(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |GTE(ex1,ex2)=> y(BigInt.ge(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2)))))
        |ID(idix)=> get_value(!globaldict,idix)
        |POSNUM(num)=> num
        end

fun booo(ex)=
        let
        in
        case ex of
        NOT(ex1)=> ynot(booo(ex1))
        |AND(ex1,ex2)=> yand(booo(ex1),booo(ex2))
        |OR(ex1,ex2)=> yand(booo(ex1),booo(ex2))
        |EQB(ex1,ex2)=> yeq(booo(ex1),booo(ex2))
        |NEQB(ex1,ex2)=> ynot(yeq(booo(ex1),booo(ex2)))
        |IDB(idix)=>get_value(!globaldict,idix)
        |TTT(_)=>"true"
        |FFF(_)=>"false"
        end

fun ratatouille(ex)=
        let
        open Rational;
        open BigInt;
        in
        case ex of 
        NEGR(ex)=> Rational.toDecimal(Rational.neg(Rational.fromDecimal(ratatouille(ex))))
        |INVERSE(ex)=> Rational.toDecimal(valOf(Rational.inverse(Rational.fromDecimal(ratatouille(ex)))))
        |ADDRAT(ex1,ex2)=>Rational.toDecimal(Rational.add(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        |SUBRAT(ex1,ex2)=>Rational.toDecimal(Rational.subtract(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        |MULRAT(ex1,ex2)=>Rational.toDecimal(Rational.multiply(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        |DIVRAT(ex1,ex2)=>Rational.toDecimal(valOf(Rational.divide(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2)))))
        |MAKERAT(ex1,ex2)=>Rational.toDecimal(valOf(Rational.make_rat(valOf(BigInt.fromString(intex(ex1))),valOf(BigInt.fromString(intex(ex2))))))
        |RAT(ex)=>Rational.toDecimal(valOf(Rational.rat(valOf(BigInt.fromString(intex(ex))))))
        |SHOWRAT(ex)=>"1"
        |SHOWDECIMAL(ex)=>"1"
        |FROMDECIMAL(ex)=>"1"
        |TODECIMAL(ex)=>"1"
        |RNUM(xyz)=>xyz
        |IDR(idix)=>get_value(!globaldict,idix)
        |EQR(ex1,ex2)=>y(Rational.equal(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        |NEQR(ex1,ex2)=>y2(Rational.equal(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        |LTR(ex1,ex2)=>y(Rational.less(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        |LTER(ex1,ex2)=>y(Rational.less(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))) orelse Rational.equal(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        |GTR(ex1,ex2)=>y2(Rational.less(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))) orelse Rational.equal(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        |GTER(ex1,ex2)=>y2(Rational.less(Rational.fromDecimal(ratatouille(ex1)),Rational.fromDecimal(ratatouille(ex2))))
        end

fun evalexpression(b)=
        let
        open BigInt
        in
        case b of
        EXP(ex)=>       let
                        val value = evalexpression(ex)
                        in value
                        end
        | IEXP(ex)=>
                        let
                        val value = intex(ex)
                        in value
                        end
        | BEXP(ex)=>
                        let
                        val value = booo(ex)
                        in value
                        end                        
        | REXP(ex)=>    
                        let
                        val value = ratatouille(ex)
                        in value
                        end
        | EQEXP(ex1,ex2)=>
                        let
                        open Rational;
                        val value = evalexpression(ex1);
                        val value2 = evalexpression(ex2);
                        in
                        if value = "true" andalso value2 = "true" then "true"
                        else if value = "false" andalso value2 = "false" then "true"
                        else if value = "true" andalso value2 = "true" then "false"
                        else if value = "false" andalso value2 = "true" then "false"
                        else y(Rational.equal(Rational.fromDecimal(value),Rational.fromDecimal(value2)))
                        end
        | NEQEX(ex1,ex2)=>
                        let
                        open Rational;
                        val value = evalexpression(ex1);
                        val value2 = evalexpression(ex2);
                        in
                        if value = "true" andalso value2 = "true" then "false"
                        else if value = "false" andalso value2 = "false" then "false"
                        else if value = "true" andalso value2 = "true" then "true"
                        else if value = "false" andalso value2 = "true" then "true"
                        else y2(Rational.equal(Rational.fromDecimal(value),Rational.fromDecimal(value2)))
                        end
        end

fun copyListPoly xs =
  let
    fun copyElem x = x
  in
    List.map copyElem xs
  end

fun setvar(a,b)=
        let
                val value = evalexpression(b)
                val new_dict = update_id(!globaldict,a,get_type(!globaldict,a),value)
        in
                globaldict := new_dict;
                1
        end


fun cmdhandle(x: CMD list)=
        if length(x)=0 then 1
        else
                let
                        val cmd1 = hd(x);
                        fun ifthenelse(a,b,c)=
                                let
                                        val value = evalexpression(a)
                                in
                                        if value = "true" then cmdhandle(b)
                                        else cmdhandle(c)
                                end
                        

                in
                        case cmd1 of
                        Call(a)=>
                                let
                                        val blk = get_proceed(!procedlist,a);
                                        val BLK(dec,cmd) = get_proceed(!procedlist,a);
                                        val newglob = ref (  copyDict (!globaldict) );
                                        val new_stack = (newglob :: !stack );
                                        val _ = (stack := new_stack );
                                        val _ = (globaldict := !(hd( ! stack)) );
                                        val _ = dechandle(dec);
                                        val _ = cmdhandle(cmd);
                                        val _ = (stack := tl(!stack));
                                        val _ = (globaldict := !(hd( ! stack)) );
                                in
 
                                        cmdhandle(tl(x))
                                        (* end *)
                                end

                        |SET(a,b)=>
                                let
                                        val _ = setvar(a,b)
                                in
                                        cmdhandle(tl(x))
                                end 
                        
                        |Read(a)=>
                                let     
                                        val _ = print(a^" : ")
                                        val putin =  TextIO.inputLine TextIO.stdIn;
                                        val lenin = strip(valOf(putin))
                                        val new_dict = update_id(!globaldict,a,get_type(!globaldict,a),lenin)
                                in
                                        globaldict := new_dict;
                                        cmdhandle(tl(x))
                                end
                        |Print(a)=>
                                let
                                        val value = evalexpression(a)
                                        val _ =  (output := value :: !output)
                                        (* val _ = print(value^"\n") *)
                                in
                                        cmdhandle(tl(x))
                                end
                        |ITE(a,b,c)=>
                                let
                                        val _ = ifthenelse(a,b,c)
                                in
                                        cmdhandle(tl(x))
                                end
                        |WH(a,b)=>
                                let 
                                        val value = evalexpression(a)
                                        val _ = ifthenelse(a,b,[])
                                in
                                        if value="true" then cmdhandle(x)
                                        else cmdhandle(tl(x))
                                end
                end

fun legoblocks(x : BLK)=
        let 
            val BLK (dec,cmd) = x;
            val _ = dechandle(dec);
            val _ = cmdhandle(cmd);
        in
                1
        end; 

fun letsgo(x : AST)=
        let
                val PROG block = x;
                val _ = legoblocks (block);
                val result = procedlist;
        in 
                (* stack *)
                (* () *)
                result
        end; 

fun extract_from_blk (BLK cmds) acc =
    let
        fun get_procs ([], acc) = acc
          | get_procs ((PDEF (procedure,_,blk))::rest, acc) =
            get_procs(rest, (procedure, blk)::acc)
          | get_procs (_::rest, acc) = get_procs(rest, acc)
        fun extract_cmds [] acc = acc
          | extract_cmds ((PDEF _)::rest) acc = extract_cmds rest acc
          | extract_cmds ((VDEF _)::rest) acc = extract_cmds rest acc
          | extract_cmds ((Read _)::rest) acc = extract_cmds rest acc
          | extract_cmds ((Print _)::rest) acc = extract_cmds rest acc
          | extract_cmds ((Call proc)::rest) acc = extract_cmds rest ((proc, BLK [])::acc)
        val nested_procs =
            List.foldr (fn ((_, BLK blk), acc) => extract_from_blk blk acc | _ => acc) [] cmds
        val proc_list = get_procs (cmds, [])
        val extracted_cmds = extract_cmds cmds []
    in
        proc_list @ nested_procs @ acc @ extracted_cmds
    end;





end;
