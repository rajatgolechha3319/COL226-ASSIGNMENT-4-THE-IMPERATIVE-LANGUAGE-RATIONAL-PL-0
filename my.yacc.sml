functor MyLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : My_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes;


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\013\000\003\000\012\000\004\000\011\000\015\000\010\000\
\\050\000\161\000\000\000\
\\001\000\002\000\044\000\003\000\043\000\004\000\042\000\048\000\041\000\000\000\
\\001\000\005\000\058\000\006\000\057\000\036\000\056\000\052\000\055\000\000\000\
\\001\000\009\000\074\000\039\000\051\000\040\000\050\000\000\000\
\\001\000\010\000\148\000\000\000\
\\001\000\011\000\156\000\000\000\
\\001\000\013\000\052\000\039\000\051\000\040\000\050\000\000\000\
\\001\000\014\000\117\000\000\000\
\\001\000\019\000\062\000\052\000\061\000\053\000\060\000\000\000\
\\001\000\019\000\073\000\020\000\072\000\025\000\071\000\026\000\070\000\
\\027\000\069\000\028\000\068\000\029\000\067\000\030\000\066\000\
\\052\000\065\000\054\000\064\000\000\000\
\\001\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\041\000\101\000\042\000\100\000\
\\043\000\099\000\044\000\098\000\049\000\149\000\000\000\
\\001\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\041\000\101\000\042\000\100\000\
\\043\000\099\000\044\000\098\000\049\000\150\000\000\000\
\\001\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\041\000\101\000\042\000\100\000\
\\043\000\099\000\044\000\098\000\049\000\151\000\000\000\
\\001\000\031\000\096\000\032\000\095\000\033\000\094\000\034\000\093\000\
\\035\000\092\000\039\000\091\000\040\000\090\000\041\000\089\000\
\\042\000\088\000\043\000\087\000\044\000\086\000\047\000\153\000\000\000\
\\001\000\031\000\096\000\032\000\095\000\033\000\094\000\034\000\093\000\
\\035\000\092\000\039\000\091\000\040\000\090\000\041\000\089\000\
\\042\000\088\000\043\000\087\000\044\000\086\000\049\000\152\000\000\000\
\\001\000\031\000\096\000\032\000\095\000\033\000\094\000\034\000\093\000\
\\035\000\092\000\039\000\091\000\040\000\090\000\041\000\089\000\
\\042\000\088\000\043\000\087\000\044\000\086\000\049\000\157\000\000\000\
\\001\000\039\000\051\000\040\000\050\000\049\000\076\000\000\000\
\\001\000\039\000\051\000\040\000\050\000\049\000\080\000\000\000\
\\001\000\045\000\036\000\000\000\
\\001\000\046\000\015\000\000\000\
\\001\000\046\000\023\000\000\000\
\\001\000\046\000\034\000\000\000\
\\001\000\048\000\037\000\000\000\
\\001\000\048\000\039\000\000\000\
\\001\000\048\000\108\000\000\000\
\\001\000\048\000\109\000\000\000\
\\001\000\048\000\110\000\000\000\
\\001\000\048\000\112\000\000\000\
\\001\000\048\000\113\000\000\000\
\\001\000\049\000\075\000\000\000\
\\001\000\050\000\020\000\000\000\
\\001\000\051\000\035\000\000\000\
\\001\000\052\000\014\000\000\000\
\\001\000\052\000\017\000\000\000\
\\001\000\052\000\038\000\000\000\
\\001\000\052\000\048\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\047\000\024\000\000\000\
\\170\000\000\000\
\\171\000\015\000\010\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\008\000\032\000\012\000\031\000\016\000\030\000\017\000\029\000\
\\018\000\028\000\052\000\027\000\000\000\
\\176\000\000\000\
\\177\000\039\000\051\000\040\000\050\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\041\000\101\000\042\000\100\000\
\\043\000\099\000\044\000\098\000\000\000\
\\184\000\031\000\096\000\032\000\095\000\033\000\094\000\034\000\093\000\
\\035\000\092\000\039\000\091\000\040\000\090\000\041\000\089\000\
\\042\000\088\000\043\000\087\000\044\000\086\000\000\000\
\\185\000\037\000\084\000\038\000\083\000\039\000\082\000\040\000\081\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\039\000\103\000\040\000\102\000\000\000\
\\190\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\041\000\101\000\042\000\100\000\
\\043\000\099\000\044\000\098\000\000\000\
\\191\000\023\000\105\000\024\000\104\000\039\000\103\000\040\000\102\000\000\000\
\\192\000\023\000\105\000\024\000\104\000\039\000\103\000\040\000\102\000\000\000\
\\193\000\039\000\103\000\040\000\102\000\000\000\
\\194\000\039\000\103\000\040\000\102\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\197\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\041\000\101\000\042\000\100\000\
\\043\000\099\000\044\000\098\000\000\000\
\\198\000\000\000\
\\199\000\000\000\
\\200\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\000\000\
\\206\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\000\000\
\\207\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\000\000\
\\208\000\021\000\107\000\022\000\106\000\023\000\105\000\024\000\104\000\
\\039\000\103\000\040\000\102\000\000\000\
\\209\000\039\000\091\000\040\000\090\000\000\000\
\\210\000\033\000\094\000\034\000\093\000\035\000\092\000\039\000\091\000\
\\040\000\090\000\000\000\
\\211\000\033\000\094\000\034\000\093\000\035\000\092\000\039\000\091\000\
\\040\000\090\000\000\000\
\\212\000\039\000\091\000\040\000\090\000\000\000\
\\213\000\039\000\091\000\040\000\090\000\000\000\
\\214\000\039\000\091\000\040\000\090\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\031\000\096\000\032\000\095\000\033\000\094\000\034\000\093\000\
\\035\000\092\000\039\000\091\000\040\000\090\000\000\000\
\\218\000\031\000\096\000\032\000\095\000\033\000\094\000\034\000\093\000\
\\035\000\092\000\039\000\091\000\040\000\090\000\000\000\
\\219\000\031\000\096\000\032\000\095\000\033\000\094\000\034\000\093\000\
\\035\000\092\000\039\000\091\000\040\000\090\000\000\000\
\\220\000\031\000\096\000\032\000\095\000\033\000\094\000\034\000\093\000\
\\035\000\092\000\039\000\091\000\040\000\090\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\039\000\082\000\040\000\081\000\000\000\
\\224\000\039\000\082\000\040\000\081\000\000\000\
\\225\000\000\000\
\\226\000\000\000\
\\227\000\039\000\082\000\040\000\081\000\000\000\
\\228\000\000\000\
\\229\000\000\000\
\\230\000\000\000\
\"
val actionRowNumbers =
"\001\000\033\000\020\000\041\000\
\\034\000\001\000\031\000\037\000\
\\045\000\043\000\042\000\044\000\
\\001\000\048\000\021\000\046\000\
\\039\000\038\000\052\000\050\000\
\\049\000\040\000\034\000\022\000\
\\032\000\019\000\023\000\035\000\
\\024\000\002\000\002\000\047\000\
\\052\000\051\000\002\000\036\000\
\\055\000\002\000\007\000\002\000\
\\003\000\009\000\010\000\004\000\
\\053\000\054\000\030\000\017\000\
\\002\000\002\000\031\000\018\000\
\\062\000\107\000\003\000\106\000\
\\105\000\061\000\099\000\098\000\
\\009\000\060\000\078\000\079\000\
\\025\000\026\000\027\000\010\000\
\\028\000\029\000\010\000\010\000\
\\031\000\056\000\057\000\065\000\
\\064\000\008\000\063\000\003\000\
\\003\000\003\000\003\000\100\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\086\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\074\000\009\000\009\000\
\\067\000\066\000\005\000\059\000\
\\103\000\102\000\104\000\101\000\
\\097\000\096\000\095\000\094\000\
\\093\000\092\000\091\000\090\000\
\\089\000\088\000\087\000\085\000\
\\084\000\083\000\082\000\081\000\
\\080\000\071\000\070\000\069\000\
\\068\000\011\000\012\000\013\000\
\\015\000\014\000\031\000\077\000\
\\076\000\075\000\073\000\009\000\
\\006\000\016\000\058\000\072\000\
\\000\000"
val gotoT =
"\
\\001\000\156\000\002\000\007\000\003\000\006\000\006\000\005\000\
\\008\000\004\000\009\000\003\000\010\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\014\000\000\000\
\\003\000\016\000\006\000\005\000\008\000\004\000\009\000\003\000\
\\010\000\002\000\013\000\001\000\000\000\
\\004\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\019\000\003\000\006\000\006\000\005\000\008\000\004\000\
\\009\000\003\000\010\000\002\000\013\000\001\000\000\000\
\\009\000\020\000\010\000\002\000\013\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\024\000\011\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\038\000\000\000\
\\012\000\043\000\000\000\
\\000\000\
\\005\000\044\000\011\000\023\000\000\000\
\\000\000\
\\012\000\045\000\000\000\
\\000\000\
\\000\000\
\\012\000\047\000\000\000\
\\000\000\
\\012\000\051\000\000\000\
\\016\000\052\000\000\000\
\\015\000\057\000\000\000\
\\014\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\075\000\000\000\
\\012\000\076\000\000\000\
\\004\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\095\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\109\000\000\000\
\\000\000\
\\000\000\
\\014\000\112\000\000\000\
\\014\000\113\000\000\000\
\\004\000\114\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\116\000\000\000\
\\016\000\117\000\000\000\
\\016\000\118\000\000\000\
\\016\000\119\000\000\000\
\\000\000\
\\015\000\120\000\000\000\
\\015\000\121\000\000\000\
\\015\000\122\000\000\000\
\\015\000\123\000\000\000\
\\015\000\124\000\000\000\
\\015\000\125\000\000\000\
\\015\000\126\000\000\000\
\\015\000\127\000\000\000\
\\015\000\128\000\000\000\
\\015\000\129\000\000\000\
\\015\000\130\000\000\000\
\\000\000\
\\014\000\131\000\000\000\
\\014\000\132\000\000\000\
\\014\000\133\000\000\000\
\\014\000\134\000\000\000\
\\014\000\135\000\000\000\
\\014\000\136\000\000\000\
\\014\000\137\000\000\000\
\\014\000\138\000\000\000\
\\014\000\139\000\000\000\
\\014\000\140\000\000\000\
\\014\000\141\000\000\000\
\\014\000\142\000\000\000\
\\014\000\143\000\000\000\
\\000\000\
\\015\000\144\000\000\000\
\\015\000\145\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\152\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\153\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 157
val numrules = 72
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | RATIONALNUM of unit ->  (string) | POSNUMERAL of unit ->  (string)
 | IDENTIFIER of unit ->  (string) | FALSE of unit ->  (string)
 | TRUE of unit ->  (string) | boolex of unit ->  (BEXP)
 | integerex of unit ->  (IEXP) | rationalex of unit ->  (REXP)
 | typedec2 of unit ->  (Type2) | expression of unit ->  (Exp)
 | command of unit ->  (CMD) | procdef of unit ->  (PDEF)
 | pdeflist of unit ->  (PDEF list) | typedec of unit ->  (Type)
 | varlist of unit ->  (string list) | dec of unit ->  (DEC)
 | commands of unit ->  (CMD list) | comseq of unit ->  (CMD list)
 | decseq of unit ->  (DEC list) | blk of unit ->  (BLK)
 | start of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "RATIONAL"
  | (T 2) => "INTEGER"
  | (T 3) => "BOOLEAN"
  | (T 4) => "TRUE"
  | (T 5) => "FALSE"
  | (T 6) => "VAR"
  | (T 7) => "IF"
  | (T 8) => "THEN"
  | (T 9) => "ELSE"
  | (T 10) => "FI"
  | (T 11) => "WHILE"
  | (T 12) => "DO"
  | (T 13) => "OD"
  | (T 14) => "PROCEDURE"
  | (T 15) => "PRINT"
  | (T 16) => "CALL"
  | (T 17) => "READ"
  | (T 18) => "NEG"
  | (T 19) => "INVERSE"
  | (T 20) => "ADDRAT"
  | (T 21) => "SUBRAT"
  | (T 22) => "MULRAT"
  | (T 23) => "DIVRAT"
  | (T 24) => "MAKERAT"
  | (T 25) => "RAT"
  | (T 26) => "SHOWRAT"
  | (T 27) => "SHOWDECIMAL"
  | (T 28) => "FROMDECIMAL"
  | (T 29) => "TODECIMAL"
  | (T 30) => "ADD"
  | (T 31) => "SUB"
  | (T 32) => "MUL"
  | (T 33) => "DIV"
  | (T 34) => "MOD"
  | (T 35) => "NOT"
  | (T 36) => "AND"
  | (T 37) => "OR"
  | (T 38) => "EQ"
  | (T 39) => "NEQ"
  | (T 40) => "LT"
  | (T 41) => "LTE"
  | (T 42) => "GT"
  | (T 43) => "GTE"
  | (T 44) => "ASSIGN"
  | (T 45) => "SEMICOLON"
  | (T 46) => "COMMA"
  | (T 47) => "LPAREN"
  | (T 48) => "RPAREN"
  | (T 49) => "LBRACE"
  | (T 50) => "RBRACE"
  | (T 51) => "IDENTIFIER"
  | (T 52) => "POSNUMERAL"
  | (T 53) => "RATIONALNUM"
  | (T 54) => "EXP"
  | (T 55) => "EQB"
  | (T 56) => "EQEXP"
  | (T 57) => "NEQEX"
  | (T 58) => "TTT"
  | (T 59) => "FFF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 59) $$ (T 58) $$ (T 57) $$ (T 56) $$ (T 55) $$ (T 54) $$ (T 50)
 $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43)
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.blk blk1, blk1left, blk1right)) :: rest671)
) => let val  result = MlyValue.start (fn _ => let val  (blk as blk1)
 = blk1 ()
 in ( PROG(blk))
end)
 in ( LrTable.NT 0, ( result, blk1left, blk1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.comseq comseq1, _, comseq1right)) :: ( _, ( 
MlyValue.decseq decseq1, decseq1left, _)) :: rest671)) => let val  
result = MlyValue.blk (fn _ => let val  (decseq as decseq1) = decseq1
 ()
 val  (comseq as comseq1) = comseq1 ()
 in (BLK(decseq, comseq))
end)
 in ( LrTable.NT 1, ( result, decseq1left, comseq1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.decseq (fn _ => (
([])))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decseq decseq1, _, decseq1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.decseq (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decseq as decseq1) = decseq1 ()
 in ( (dec::decseq))
end)
 in ( LrTable.NT 2, ( result, dec1left, decseq1right), rest671)
end
|  ( 4, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( MlyValue.typedec typedec1, typedec1left, _)
) :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
typedec as typedec1) = typedec1 ()
 val  (varlist as varlist1) = varlist1 ()
 in (VDEC(typedec,varlist))
end)
 in ( LrTable.NT 5, ( result, typedec1left, SEMICOLON1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.pdeflist pdeflist1, pdeflist1left, 
pdeflist1right)) :: rest671)) => let val  result = MlyValue.dec (fn _
 => let val  (pdeflist as pdeflist1) = pdeflist1 ()
 in (PDEC(pdeflist))
end)
 in ( LrTable.NT 5, ( result, pdeflist1left, pdeflist1right), rest671)

end
|  ( 6, ( ( _, ( _, INTEGER1left, INTEGER1right)) :: rest671)) => let
 val  result = MlyValue.typedec (fn _ => ((Int)))
 in ( LrTable.NT 7, ( result, INTEGER1left, INTEGER1right), rest671)

end
|  ( 7, ( ( _, ( _, BOOLEAN1left, BOOLEAN1right)) :: rest671)) => let
 val  result = MlyValue.typedec (fn _ => ((Bool)))
 in ( LrTable.NT 7, ( result, BOOLEAN1left, BOOLEAN1right), rest671)

end
|  ( 8, ( ( _, ( _, RATIONAL1left, RATIONAL1right)) :: rest671)) =>
 let val  result = MlyValue.typedec (fn _ => ((Rational)))
 in ( LrTable.NT 7, ( result, RATIONAL1left, RATIONAL1right), rest671)

end
|  ( 9, ( ( _, ( _, PROCEDURE1left, PROCEDURE1right)) :: rest671)) =>
 let val  result = MlyValue.typedec2 (fn _ => ((procedure)))
 in ( LrTable.NT 12, ( result, PROCEDURE1left, PROCEDURE1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.varlist
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (([IDENTIFIER]))
end)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _
 :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: 
rest671)) => let val  result = MlyValue.varlist (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (varlist as varlist1) = varlist1 ()
 in ((IDENTIFIER::varlist))
end)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, varlist1right), rest671
)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.pdeflist (fn _ => (
([])))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( MlyValue.pdeflist pdeflist1, _, pdeflist1right)) ::
 _ :: ( _, ( MlyValue.procdef procdef1, procdef1left, _)) :: rest671))
 => let val  result = MlyValue.pdeflist (fn _ => let val  (procdef as 
procdef1) = procdef1 ()
 val  (pdeflist as pdeflist1) = pdeflist1 ()
 in ((procdef::pdeflist))
end)
 in ( LrTable.NT 8, ( result, procdef1left, pdeflist1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.blk blk1, _, blk1right)) :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( MlyValue.typedec2 
typedec21, typedec21left, _)) :: rest671)) => let val  result = 
MlyValue.procdef (fn _ => let val  (typedec2 as typedec21) = typedec21
 ()
 val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (blk as blk1) = blk1 ()
 in (PDEF(typedec2,IDENTIFIER,blk))
end)
 in ( LrTable.NT 9, ( result, typedec21left, blk1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.commands 
commands1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.comseq (fn _ => let val  (commands as 
commands1) = commands1 ()
 in ((commands))
end)
 in ( LrTable.NT 3, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 16, ( rest671)) => let val  result = MlyValue.commands (fn _ => (
([])))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.commands commands1, _, commands1right)) ::
 _ :: ( _, ( MlyValue.command command1, command1left, _)) :: rest671))
 => let val  result = MlyValue.commands (fn _ => let val  (command as 
command1) = command1 ()
 val  (commands as commands1) = commands1 ()
 in ((command::commands))
end)
 in ( LrTable.NT 4, ( result, command1left, commands1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)
) :: rest671)) => let val  result = MlyValue.command (fn _ => let val 
 (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (expression as expression1) = expression1 ()
 in (SET(IDENTIFIER,expression))
end)
 in ( LrTable.NT 10, ( result, IDENTIFIER1left, expression1right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, IDENTIFIER1right
)) :: ( _, ( _, CALL1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in (Call(IDENTIFIER))
end)
 in ( LrTable.NT 10, ( result, CALL1left, IDENTIFIER1right), rest671)

end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.IDENTIFIER 
IDENTIFIER1, _, _)) :: _ :: ( _, ( _, READ1left, _)) :: rest671)) =>
 let val  result = MlyValue.command (fn _ => let val  (IDENTIFIER as 
IDENTIFIER1) = IDENTIFIER1 ()
 in (Read(IDENTIFIER))
end)
 in ( LrTable.NT 10, ( result, READ1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.command (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (Print(expression))
end)
 in ( LrTable.NT 10, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.comseq comseq2,
 _, _)) :: _ :: ( _, ( MlyValue.comseq comseq1, _, _)) :: _ :: ( _, ( 
MlyValue.expression expression1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.command (fn _ => let val  (
expression as expression1) = expression1 ()
 val  comseq1 = comseq1 ()
 val  comseq2 = comseq2 ()
 in (ITE(expression,comseq1,comseq2))
end)
 in ( LrTable.NT 10, ( result, IF1left, FI1right), rest671)
end
|  ( 23, ( ( _, ( _, _, OD1right)) :: ( _, ( MlyValue.comseq comseq1,
 _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _)) :: ( _,
 ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 val  (comseq as comseq1) = comseq1 ()
 in (WH(expression,comseq))
end)
 in ( LrTable.NT 10, ( result, WHILE1left, OD1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.rationalex rationalex1, _, rationalex1right
)) :: ( _, ( _, RATIONAL1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (rationalex as rationalex1) = 
rationalex1 ()
 in (REXP(rationalex))
end)
 in ( LrTable.NT 11, ( result, RATIONAL1left, rationalex1right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.integerex integerex1, _, integerex1right))
 :: ( _, ( _, INTEGER1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (integerex as integerex1) = 
integerex1 ()
 in (IEXP(integerex))
end)
 in ( LrTable.NT 11, ( result, INTEGER1left, integerex1right), rest671
)
end
|  ( 26, ( ( _, ( MlyValue.boolex boolex1, _, boolex1right)) :: ( _, (
 _, BOOLEAN1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (boolex as boolex1) = boolex1 ()
 in (BEXP(boolex))
end)
 in ( LrTable.NT 11, ( result, BOOLEAN1left, boolex1right), rest671)

end
|  ( 27, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (EXP(expression))
end)
 in ( LrTable.NT 11, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (EQEXP(expression1,expression2))
end)
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (NEQEX(expression1,expression2))
end)
 in ( LrTable.NT 11, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.rationalex rationalex1, _, rationalex1right
)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.rationalex (fn _ => let val  (rationalex as rationalex1) = 
rationalex1 ()
 in (NEGR(rationalex))
end)
 in ( LrTable.NT 13, ( result, NEG1left, rationalex1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.rationalex rationalex1, _, rationalex1right
)) :: ( _, ( _, INVERSE1left, _)) :: rest671)) => let val  result = 
MlyValue.rationalex (fn _ => let val  (rationalex as rationalex1) = 
rationalex1 ()
 in (INVERSE(rationalex))
end)
 in ( LrTable.NT 13, ( result, INVERSE1left, rationalex1right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (ADDRAT(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (SUBRAT(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (MULRAT(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (DIVRAT(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 36, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.integerex 
integerex2, _, _)) :: _ :: ( _, ( MlyValue.integerex integerex1, _, _)
) :: _ :: ( _, ( _, MAKERAT1left, _)) :: rest671)) => let val  result
 = MlyValue.rationalex (fn _ => let val  integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (MAKERAT(integerex1,integerex2))
end)
 in ( LrTable.NT 13, ( result, MAKERAT1left, RPAREN1right), rest671)

end
|  ( 37, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.integerex 
integerex1, _, _)) :: _ :: ( _, ( _, RAT1left, _)) :: rest671)) => let
 val  result = MlyValue.rationalex (fn _ => let val  (integerex as 
integerex1) = integerex1 ()
 in (RAT(integerex))
end)
 in ( LrTable.NT 13, ( result, RAT1left, RPAREN1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.rationalex rationalex1, _, rationalex1right
)) :: ( _, ( _, SHOWRAT1left, _)) :: rest671)) => let val  result = 
MlyValue.rationalex (fn _ => let val  (rationalex as rationalex1) = 
rationalex1 ()
 in (SHOWRAT(rationalex))
end)
 in ( LrTable.NT 13, ( result, SHOWRAT1left, rationalex1right), 
rest671)
end
|  ( 39, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.rationalex 
rationalex1, _, _)) :: _ :: ( _, ( _, SHOWDECIMAL1left, _)) :: rest671
)) => let val  result = MlyValue.rationalex (fn _ => let val  (
rationalex as rationalex1) = rationalex1 ()
 in (SHOWDECIMAL(rationalex))
end)
 in ( LrTable.NT 13, ( result, SHOWDECIMAL1left, RPAREN1right), 
rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.rationalex 
rationalex1, _, _)) :: _ :: ( _, ( _, FROMDECIMAL1left, _)) :: rest671
)) => let val  result = MlyValue.rationalex (fn _ => let val  (
rationalex as rationalex1) = rationalex1 ()
 in (FROMDECIMAL(rationalex))
end)
 in ( LrTable.NT 13, ( result, FROMDECIMAL1left, RPAREN1right), 
rest671)
end
|  ( 41, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.rationalex 
rationalex1, _, _)) :: _ :: ( _, ( _, TODECIMAL1left, _)) :: rest671))
 => let val  result = MlyValue.rationalex (fn _ => let val  (
rationalex as rationalex1) = rationalex1 ()
 in (TODECIMAL(rationalex))
end)
 in ( LrTable.NT 13, ( result, TODECIMAL1left, RPAREN1right), rest671)

end
|  ( 42, ( ( _, ( MlyValue.RATIONALNUM RATIONALNUM1, RATIONALNUM1left,
 RATIONALNUM1right)) :: rest671)) => let val  result = 
MlyValue.rationalex (fn _ => let val  (RATIONALNUM as RATIONALNUM1) = 
RATIONALNUM1 ()
 in ((RNUM(RATIONALNUM)))
end)
 in ( LrTable.NT 13, ( result, RATIONALNUM1left, RATIONALNUM1right), 
rest671)
end
|  ( 43, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = 
MlyValue.rationalex (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in ((IDR(IDENTIFIER)))
end)
 in ( LrTable.NT 13, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 44, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (EQR(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 45, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (NEQR(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 46, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (LTR(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 47, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (LTER(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 48, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (GTR(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 49, ( ( _, ( MlyValue.rationalex rationalex2, _, rationalex2right
)) :: _ :: ( _, ( MlyValue.rationalex rationalex1, rationalex1left, _)
) :: rest671)) => let val  result = MlyValue.rationalex (fn _ => let
 val  rationalex1 = rationalex1 ()
 val  rationalex2 = rationalex2 ()
 in (GTER(rationalex1,rationalex2))
end)
 in ( LrTable.NT 13, ( result, rationalex1left, rationalex2right), 
rest671)
end
|  ( 50, ( ( _, ( MlyValue.integerex integerex1, _, integerex1right))
 :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.integerex (fn _ => let val  (integerex as integerex1) = 
integerex1 ()
 in (NEG(integerex))
end)
 in ( LrTable.NT 14, ( result, NEG1left, integerex1right), rest671)

end
|  ( 51, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (ADD(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 52, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (SUB(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 53, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (MUL(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 54, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (DIV(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 55, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (MOD(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 56, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (EQ(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 57, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (NEQ(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 58, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (LT(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 59, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (LTE(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 60, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (GT(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 61, ( ( _, ( MlyValue.integerex integerex2, _, integerex2right))
 :: _ :: ( _, ( MlyValue.integerex integerex1, integerex1left, _)) :: 
rest671)) => let val  result = MlyValue.integerex (fn _ => let val  
integerex1 = integerex1 ()
 val  integerex2 = integerex2 ()
 in (GTE(integerex1,integerex2))
end)
 in ( LrTable.NT 14, ( result, integerex1left, integerex2right), 
rest671)
end
|  ( 62, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = 
MlyValue.integerex (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in ((ID(IDENTIFIER)))
end)
 in ( LrTable.NT 14, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 63, ( ( _, ( MlyValue.POSNUMERAL POSNUMERAL1, POSNUMERAL1left, 
POSNUMERAL1right)) :: rest671)) => let val  result = 
MlyValue.integerex (fn _ => let val  (POSNUMERAL as POSNUMERAL1) = 
POSNUMERAL1 ()
 in ((POSNUM(POSNUMERAL)))
end)
 in ( LrTable.NT 14, ( result, POSNUMERAL1left, POSNUMERAL1right), 
rest671)
end
|  ( 64, ( ( _, ( MlyValue.boolex boolex1, _, boolex1right)) :: ( _, (
 _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.boolex
 (fn _ => let val  (boolex as boolex1) = boolex1 ()
 in (NOT(boolex))
end)
 in ( LrTable.NT 15, ( result, NOT1left, boolex1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.boolex boolex2, _, boolex2right)) :: _ :: (
 _, ( MlyValue.boolex boolex1, boolex1left, _)) :: rest671)) => let
 val  result = MlyValue.boolex (fn _ => let val  boolex1 = boolex1 ()
 val  boolex2 = boolex2 ()
 in (AND(boolex1,boolex2))
end)
 in ( LrTable.NT 15, ( result, boolex1left, boolex2right), rest671)

end
|  ( 66, ( ( _, ( MlyValue.boolex boolex2, _, boolex2right)) :: _ :: (
 _, ( MlyValue.boolex boolex1, boolex1left, _)) :: rest671)) => let
 val  result = MlyValue.boolex (fn _ => let val  boolex1 = boolex1 ()
 val  boolex2 = boolex2 ()
 in (EQB(boolex1,boolex2))
end)
 in ( LrTable.NT 15, ( result, boolex1left, boolex2right), rest671)

end
|  ( 67, ( ( _, ( MlyValue.boolex boolex2, _, boolex2right)) :: _ :: (
 _, ( MlyValue.boolex boolex1, boolex1left, _)) :: rest671)) => let
 val  result = MlyValue.boolex (fn _ => let val  boolex1 = boolex1 ()
 val  boolex2 = boolex2 ()
 in (NEQB(boolex1,boolex2))
end)
 in ( LrTable.NT 15, ( result, boolex1left, boolex2right), rest671)

end
|  ( 68, ( ( _, ( MlyValue.boolex boolex2, _, boolex2right)) :: _ :: (
 _, ( MlyValue.boolex boolex1, boolex1left, _)) :: rest671)) => let
 val  result = MlyValue.boolex (fn _ => let val  boolex1 = boolex1 ()
 val  boolex2 = boolex2 ()
 in (OR(boolex1,boolex2))
end)
 in ( LrTable.NT 15, ( result, boolex1left, boolex2right), rest671)

end
|  ( 69, ( ( _, ( MlyValue.TRUE TRUE1, TRUE1left, TRUE1right)) :: 
rest671)) => let val  result = MlyValue.boolex (fn _ => let val  (TRUE
 as TRUE1) = TRUE1 ()
 in (TTT(TRUE))
end)
 in ( LrTable.NT 15, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.FALSE FALSE1, FALSE1left, FALSE1right)) :: 
rest671)) => let val  result = MlyValue.boolex (fn _ => let val  (
FALSE as FALSE1) = FALSE1 ()
 in (FFF(FALSE))
end)
 in ( LrTable.NT 15, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.boolex
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in ((IDB(IDENTIFIER)))
end)
 in ( LrTable.NT 15, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : My_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun RATIONAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLEAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.TRUE (fn () => i),p1,p2))
fun FALSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.FALSE (fn () => i),p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun ADDRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun SUBRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun MULRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun FROMDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TODECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun LTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun GTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
fun POSNUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.POSNUMERAL (fn () => i),p1,p2))
fun RATIONALNUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.RATIONALNUM (fn () => i),p1,p2))
fun EXP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(
ParserData.MlyValue.VOID,p1,p2))
fun EQB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 55,(
ParserData.MlyValue.VOID,p1,p2))
fun EQEXP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 56,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQEX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 57,(
ParserData.MlyValue.VOID,p1,p2))
fun TTT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 58,(
ParserData.MlyValue.VOID,p1,p2))
fun FFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 59,(
ParserData.MlyValue.VOID,p1,p2))
end
end
