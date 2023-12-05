structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor MyLexFun(structure Tokens: My_TOKENS));
%s RATIONAL_PL0 COMMENT;
alpha=[A-Za-z];
digit=[0-9];
ascii=[\x20-\x7F];
ws = [\ \t];
%%
<INITIAL>{ws}*    => (YYBEGIN RATIONAL_PL0; continue());
<RATIONAL_PL0>{ws}+    => (continue());
<RATIONAL_PL0>\n       => ( pos:= (!pos) + 1; lex()); 
<COMMENT>\n       => ( pos:= (!pos) + 1; lex()); 
<RATIONAL_PL0>"(*" => (YYBEGIN COMMENT; continue());
<RATIONAL_PL0>"rational" => (Tokens.RATIONAL(!pos , !pos )) ;
<RATIONAL_PL0>"integer" => (Tokens.INTEGER(!pos , !pos )) ;
<RATIONAL_PL0>"boolean" => (Tokens.BOOLEAN(!pos , !pos )) ;
<RATIONAL_PL0>"tt" => (Tokens.TRUE(yytext, !pos , !pos )) ;
<RATIONAL_PL0>"ff" => (Tokens.FALSE(yytext, !pos , !pos )) ;
<RATIONAL_PL0>"var" => (Tokens.VAR(!pos , !pos )) ;  
<RATIONAL_PL0>"if" => (Tokens.IF(!pos , !pos )) ;
<RATIONAL_PL0>"then" => (Tokens.THEN(!pos , !pos )) ;
<RATIONAL_PL0>"else" => (Tokens.ELSE(!pos , !pos )) ;
<RATIONAL_PL0>"fi" => (Tokens.FI(!pos , !pos )) ;
<RATIONAL_PL0>"while" => (Tokens.WHILE(!pos , !pos )) ;
<RATIONAL_PL0>"do" => (Tokens.DO(!pos , !pos )) ;
<RATIONAL_PL0>"od" => (Tokens.OD(!pos , !pos )) ;
<RATIONAL_PL0>"procedure" => (Tokens.PROCEDURE(!pos , !pos )) ;
<RATIONAL_PL0>"print" => (Tokens.PRINT(!pos , !pos )) ;
<RATIONAL_PL0>"call" => (Tokens.CALL(!pos , !pos )) ;
<RATIONAL_PL0>"read" => (Tokens.READ(!pos , !pos )) ;
<RATIONAL_PL0>"~" => (Tokens.NEG(!pos , !pos )) ;
<RATIONAL_PL0>"inverse" => (Tokens.INVERSE(!pos , !pos )) ;
<RATIONAL_PL0>".+." => (Tokens.ADDRAT(!pos , !pos )) ;
<RATIONAL_PL0>".-." => (Tokens.SUBRAT(!pos , !pos )) ;
<RATIONAL_PL0>".*." => (Tokens.MULRAT(!pos , !pos )) ;
<RATIONAL_PL0>"./." => (Tokens.DIVRAT(!pos , !pos )) ;
<RATIONAL_PL0>"make_rat" => (Tokens.MAKERAT(!pos , !pos )) ;
<RATIONAL_PL0>"rat" => (Tokens.RAT(!pos , !pos )) ;
<RATIONAL_PL0>"showRat" => (Tokens.SHOWRAT(!pos , !pos )) ;
<RATIONAL_PL0>"showDecimal" => (Tokens.SHOWDECIMAL(!pos , !pos )) ;
<RATIONAL_PL0>"fromDecimal" => (Tokens.FROMDECIMAL(!pos , !pos )) ;
<RATIONAL_PL0>"toDecimal" => (Tokens.TODECIMAL(!pos , !pos )) ;
<RATIONAL_PL0>"+" => (Tokens.ADD(!pos , !pos )) ;
<RATIONAL_PL0>"-" => (Tokens.SUB(!pos , !pos )) ;
<RATIONAL_PL0>"*" => (Tokens.MUL(!pos , !pos )) ;
<RATIONAL_PL0>"/" => (Tokens.DIV(!pos , !pos )) ;
<RATIONAL_PL0>"%" => (Tokens.MOD(!pos , !pos )) ;
<RATIONAL_PL0>"!" => (Tokens.NOT(!pos , !pos )) ;
<RATIONAL_PL0>"&&" => (Tokens.AND(!pos , !pos )) ;
<RATIONAL_PL0>"||" => (Tokens.OR(!pos , !pos )) ;
<RATIONAL_PL0>"=" => (Tokens.EQ(!pos , !pos )) ;
<RATIONAL_PL0>"<>" => (Tokens.NEQ(!pos , !pos )) ;
<RATIONAL_PL0>"<" => (Tokens.LT(!pos , !pos )) ;
<RATIONAL_PL0>"<=" => (Tokens.LTE(!pos , !pos )) ;
<RATIONAL_PL0>">" => (Tokens.GT(!pos , !pos )) ;
<RATIONAL_PL0>">=" => (Tokens.GTE(!pos , !pos )) ;
<RATIONAL_PL0>":=" => (Tokens.ASSIGN(!pos , !pos )) ;
<RATIONAL_PL0>";"      => (Tokens.SEMICOLON(!pos , !pos )) ;
<RATIONAL_PL0>","      => (Tokens.COMMA(!pos , !pos )) ;
<RATIONAL_PL0>"("      => (Tokens.LPAREN(!pos , !pos ));
<RATIONAL_PL0>")"      => (Tokens.RPAREN(!pos , !pos )) ; 
<RATIONAL_PL0>"}"      => (Tokens.RBRACE(!pos , !pos )) ; 
<RATIONAL_PL0>"{"      => (Tokens.LBRACE(!pos , !pos )) ; 
<RATIONAL_PL0>([+]?)({digit}){digit}* => ( Tokens.POSNUMERAL( yytext , !pos , !pos ) ) ; 
<RATIONAL_PL0>{digit}+"."{digit}*"("{digit}+")" => ( Tokens.RATIONALNUM( yytext , !pos , !pos ) ) ;
<RATIONAL_PL0>{alpha}({alpha} | {digit})* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 
<COMMENT>. =>(continue());
<COMMENT>"*)" =>(YYBEGIN RATIONAL_PL0; continue());