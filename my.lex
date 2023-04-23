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
alpha=[A-Za-z];
digit=[0-9];
ascii=[\x20-\x7F];
ws = [\ \t];
%%
\n       => ( pos:= (!pos) + 1; lex()); 
{ws}+    => (lex());
"rational" => (Tokens.RATIONAL(!pos , !pos )) ;
"integer" => (Tokens.INTEGER(!pos , !pos )) ;
"boolean" => (Tokens.BOOLEAN(!pos , !pos )) ;

"tt" => (Tokens.TRUE(yytext, !pos , !pos )) ;
"ff" => (Tokens.FALSE(yytext, !pos , !pos )) ;
"var" => (Tokens.VAR(!pos , !pos )) ;  
"if" => (Tokens.IF(!pos , !pos )) ;
"then" => (Tokens.THEN(!pos , !pos )) ;
"else" => (Tokens.ELSE(!pos , !pos )) ;
"fi" => (Tokens.FI(!pos , !pos )) ;
"while" => (Tokens.WHILE(!pos , !pos )) ;
"do" => (Tokens.DO(!pos , !pos )) ;
"od" => (Tokens.OD(!pos , !pos )) ;
"procedure" => (Tokens.PROCEDURE(!pos , !pos )) ;
"print" => (Tokens.PRINT(!pos , !pos )) ;
"call" => (Tokens.CALL(!pos , !pos )) ;
"read" => (Tokens.READ(!pos , !pos )) ;
"~" => (Tokens.NEG(!pos , !pos )) ;
"inverse" => (Tokens.INVERSE(!pos , !pos )) ;
".+." => (Tokens.ADDRAT(!pos , !pos )) ;
".-." => (Tokens.SUBRAT(!pos , !pos )) ;
".*." => (Tokens.MULRAT(!pos , !pos )) ;
"./." => (Tokens.DIVRAT(!pos , !pos )) ;
"make_rat" => (Tokens.MAKERAT(!pos , !pos )) ;
"rat" => (Tokens.RAT(!pos , !pos )) ;
"showRat" => (Tokens.SHOWRAT(!pos , !pos )) ;
"showDecimal" => (Tokens.SHOWDECIMAL(!pos , !pos )) ;
"fromDecimal" => (Tokens.FROMDECIMAL(!pos , !pos )) ;
"toDecimal" => (Tokens.TODECIMAL(!pos , !pos )) ;
"+" => (Tokens.ADD(!pos , !pos )) ;
"-" => (Tokens.SUB(!pos , !pos )) ;
"*" => (Tokens.MUL(!pos , !pos )) ;
"/" => (Tokens.DIV(!pos , !pos )) ;
"%" => (Tokens.MOD(!pos , !pos )) ;
"!" => (Tokens.NOT(!pos , !pos )) ;
"&&" => (Tokens.AND(!pos , !pos )) ;
"||" => (Tokens.OR(!pos , !pos )) ;
"=" => (Tokens.EQ(!pos , !pos )) ;
"<>" => (Tokens.NEQ(!pos , !pos )) ;
"<" => (Tokens.LT(!pos , !pos )) ;
"<=" => (Tokens.LTE(!pos , !pos )) ;
">" => (Tokens.GT(!pos , !pos )) ;
">=" => (Tokens.GTE(!pos , !pos )) ;
":=" => (Tokens.ASSIGN(!pos , !pos )) ;
";"      => (Tokens.SEMICOLON(!pos , !pos )) ;
","      => (Tokens.COMMA(!pos , !pos )) ;
"("      => (Tokens.LPAREN(!pos , !pos ));
")"      => (Tokens.RPAREN(!pos , !pos )) ; 
"}"      => (Tokens.RBRACE(!pos , !pos )) ; 
"{"      => (Tokens.LBRACE(!pos , !pos )) ; 
([+]?)({digit}){digit}* => ( Tokens.POSNUMERAL( yytext , !pos , !pos ) ) ; 
{digit}+"."{digit}*"("{digit}+")" => ( Tokens.RATIONALNUM( yytext , !pos , !pos ) ) ;
{alpha}({alpha} | {digit})* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 
