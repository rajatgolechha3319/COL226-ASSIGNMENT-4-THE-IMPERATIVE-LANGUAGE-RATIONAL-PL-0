functor MyLexFun(structure Tokens: My_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
RATIONAL_PL0 | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\t",#"\t",3),
(#" ",#" ",3),
(#"\n",#"\n",4),
(#"!",#"!",5),
(#"%",#"%",6),
(#"&",#"&",7),
(#"(",#"(",8),
(#")",#")",9),
(#"*",#"*",10),
(#"+",#"+",11),
(#",",#",",12),
(#"-",#"-",13),
(#".",#".",14),
(#"/",#"/",15),
(#"0",#"9",16),
(#":",#":",17),
(#";",#";",18),
(#"<",#"<",19),
(#"=",#"=",20),
(#">",#">",21),
(#"A",#"Z",22),
(#"a",#"a",22),
(#"g",#"h",22),
(#"j",#"l",22),
(#"n",#"n",22),
(#"q",#"q",22),
(#"u",#"u",22),
(#"x",#"z",22),
(#"b",#"b",23),
(#"c",#"c",24),
(#"d",#"d",25),
(#"e",#"e",26),
(#"f",#"f",27),
(#"i",#"i",28),
(#"m",#"m",29),
(#"o",#"o",30),
(#"p",#"p",31),
(#"r",#"r",32),
(#"s",#"s",33),
(#"t",#"t",34),
(#"v",#"v",35),
(#"w",#"w",36),
(#"{",#"{",37),
(#"|",#"|",38),
(#"}",#"}",39),
(#"~",#"~",40)], []), ([(#"\^@",#"\t",158),
(#"\v",#")",158),
(#"+",#"\255",158),
(#"\n",#"\n",159),
(#"*",#"*",160)], []), ([(#"\t",#"\t",2),
(#" ",#" ",2)], [0]), ([(#"\t",#"\t",3),
(#" ",#" ",3)], [1]), ([], [2]), ([], [39]), ([], [38]), ([(#"&",#"&",157)], []), ([(#"*",#"*",156)], [51]), ([], [52]), ([], [36]), ([(#"0",#"9",155)], [34]), ([], [50]), ([], [35]), ([(#"*",#"*",147),
(#"+",#"+",148),
(#"-",#"-",149),
(#"/",#"/",150)], []), ([], [37]), ([(#".",#".",143),
(#"0",#"9",16)], [55]), ([(#"=",#"=",142)], []), ([], [49]), ([(#"=",#"=",140),
(#">",#">",141)], [44]), ([], [42]), ([(#"=",#"=",139)], [46]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"n",22),
(#"p",#"z",22),
(#"o",#"o",133)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",130)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"n",22),
(#"p",#"z",22),
(#"o",#"o",129)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",126)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"e",22),
(#"g",#"h",22),
(#"j",#"q",22),
(#"s",#"z",22),
(#"f",#"f",114),
(#"i",#"i",115),
(#"r",#"r",116)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"e",22),
(#"g",#"m",22),
(#"o",#"z",22),
(#"f",#"f",102),
(#"n",#"n",103)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",95)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"c",22),
(#"e",#"z",22),
(#"d",#"d",94)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"q",22),
(#"s",#"z",22),
(#"r",#"r",83)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"d",22),
(#"f",#"z",22),
(#"a",#"a",73),
(#"e",#"e",74)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"g",22),
(#"i",#"z",22),
(#"h",#"h",60)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"g",22),
(#"i",#"n",22),
(#"p",#"s",22),
(#"u",#"z",22),
(#"h",#"h",48),
(#"o",#"o",49),
(#"t",#"t",50)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",46)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"g",22),
(#"i",#"z",22),
(#"h",#"h",42)], [57]), ([], [54]), ([(#"|",#"|",41)], []), ([], [53]), ([], [22]), ([], [41]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"h",22),
(#"j",#"z",22),
(#"i",#"i",43)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",44)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",45)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [15, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"q",22),
(#"s",#"z",22),
(#"r",#"r",47)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [10, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",58)], [57]), ([(#"0",#"9",22),
(#"A",#"C",22),
(#"E",#"Z",22),
(#"a",#"z",22),
(#"D",#"D",51)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [8, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",52)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"b",22),
(#"d",#"z",22),
(#"c",#"c",53)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"h",22),
(#"j",#"z",22),
(#"i",#"i",54)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"l",22),
(#"n",#"z",22),
(#"m",#"m",55)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",56)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",57)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [33, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"m",22),
(#"o",#"z",22),
(#"n",#"n",59)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [12, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"n",22),
(#"p",#"z",22),
(#"o",#"o",61)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"v",22),
(#"x",#"z",22),
(#"w",#"w",62)], [57]), ([(#"0",#"9",22),
(#"A",#"C",22),
(#"E",#"Q",22),
(#"S",#"Z",22),
(#"a",#"z",22),
(#"D",#"D",63),
(#"R",#"R",64)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",67)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",65)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"s",22),
(#"u",#"z",22),
(#"t",#"t",66)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [30, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"b",22),
(#"d",#"z",22),
(#"c",#"c",68)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"h",22),
(#"j",#"z",22),
(#"i",#"i",69)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"l",22),
(#"n",#"z",22),
(#"m",#"m",70)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",71)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",72)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [31, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"s",22),
(#"u",#"z",22),
(#"t",#"t",77)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",75)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"c",22),
(#"e",#"z",22),
(#"d",#"d",76)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [21, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"h",22),
(#"j",#"z",22),
(#"i",#"i",78)], [29, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"n",22),
(#"p",#"z",22),
(#"o",#"o",79)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"m",22),
(#"o",#"z",22),
(#"n",#"n",80)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",81)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",82)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [5, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"h",22),
(#"j",#"n",22),
(#"p",#"z",22),
(#"i",#"i",84),
(#"o",#"o",85)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"m",22),
(#"o",#"z",22),
(#"n",#"n",92)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"b",22),
(#"d",#"z",22),
(#"c",#"c",86)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",87)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"c",22),
(#"e",#"z",22),
(#"d",#"d",88)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"t",22),
(#"v",#"z",22),
(#"u",#"u",89)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"q",22),
(#"s",#"z",22),
(#"r",#"r",90)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",91)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [18, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"s",22),
(#"u",#"z",22),
(#"t",#"t",93)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [19, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [17, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"j",22),
(#"l",#"z",22),
(#"k",#"k",96)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",97)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22),
(#"_",#"_",98)], [57]), ([(#"r",#"r",99)], []), ([(#"a",#"a",100)], []), ([(#"t",#"t",101)], []), ([], [28]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [11, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"s",22),
(#"u",#"u",22),
(#"w",#"z",22),
(#"t",#"t",104),
(#"v",#"v",105)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",110)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",106)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"q",22),
(#"s",#"z",22),
(#"r",#"r",107)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"r",22),
(#"t",#"z",22),
(#"s",#"s",108)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",109)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [23, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"f",22),
(#"h",#"z",22),
(#"g",#"g",111)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",112)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"q",22),
(#"s",#"z",22),
(#"r",#"r",113)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [6, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [9, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [14, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"n",22),
(#"p",#"z",22),
(#"o",#"o",117)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"l",22),
(#"n",#"z",22),
(#"m",#"m",118)], [57]), ([(#"0",#"9",22),
(#"A",#"C",22),
(#"E",#"Z",22),
(#"a",#"z",22),
(#"D",#"D",119)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",120)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"b",22),
(#"d",#"z",22),
(#"c",#"c",121)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"h",22),
(#"j",#"z",22),
(#"i",#"i",122)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"l",22),
(#"n",#"z",22),
(#"m",#"m",123)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",124)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",125)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [32, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"r",22),
(#"t",#"z",22),
(#"s",#"s",127)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",128)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [13, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [16, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",131)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",132)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [20, 57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"n",22),
(#"p",#"z",22),
(#"o",#"o",134)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"k",22),
(#"m",#"z",22),
(#"l",#"l",135)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"d",22),
(#"f",#"z",22),
(#"e",#"e",136)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"b",#"z",22),
(#"a",#"a",137)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"m",22),
(#"o",#"z",22),
(#"n",#"n",138)], [57]), ([(#"0",#"9",22),
(#"A",#"Z",22),
(#"a",#"z",22)], [7, 57]), ([], [47]), ([], [45]), ([], [43]), ([], [48]), ([(#"(",#"(",144),
(#"0",#"9",143)], []), ([(#"0",#"9",145)], []), ([(#")",#")",146),
(#"0",#"9",145)], []), ([], [56]), ([(#".",#".",154)], []), ([(#".",#".",153)], []), ([(#".",#".",152)], []), ([(#".",#".",151)], []), ([], [27]), ([], [25]), ([], [24]), ([], [26]), ([(#"0",#"9",155)], [55]), ([], [4]), ([], [40]), ([], [58]), ([], [3]), ([(#")",#")",161)], [58]), ([], [59])]
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN RATIONAL_PL0; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( pos:= (!pos) + 1; lex()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      ( pos:= (!pos) + 1; lex()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT; continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RATIONAL(!pos , !pos )))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INTEGER(!pos , !pos )))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.BOOLEAN(!pos , !pos )))
fun yyAction8 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.TRUE(yytext, !pos , !pos ))
      end
fun yyAction9 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (Tokens.FALSE(yytext, !pos , !pos ))
      end
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.VAR(!pos , !pos )))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.IF(!pos , !pos )))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.THEN(!pos , !pos )))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ELSE(!pos , !pos )))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FI(!pos , !pos )))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.WHILE(!pos , !pos )))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DO(!pos , !pos )))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OD(!pos , !pos )))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PROCEDURE(!pos , !pos )))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.PRINT(!pos , !pos )))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.CALL(!pos , !pos )))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.READ(!pos , !pos )))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEG(!pos , !pos )))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.INVERSE(!pos , !pos )))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ADDRAT(!pos , !pos )))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SUBRAT(!pos , !pos )))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MULRAT(!pos , !pos )))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIVRAT(!pos , !pos )))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MAKERAT(!pos , !pos )))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RAT(!pos , !pos )))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOWRAT(!pos , !pos )))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SHOWDECIMAL(!pos , !pos )))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.FROMDECIMAL(!pos , !pos )))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.TODECIMAL(!pos , !pos )))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ADD(!pos , !pos )))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SUB(!pos , !pos )))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MUL(!pos , !pos )))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.DIV(!pos , !pos )))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.MOD(!pos , !pos )))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NOT(!pos , !pos )))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.AND(!pos , !pos )))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.OR(!pos , !pos )))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.EQ(!pos , !pos )))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.NEQ(!pos , !pos )))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LT(!pos , !pos )))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LTE(!pos , !pos )))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GT(!pos , !pos )))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.GTE(!pos , !pos )))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.ASSIGN(!pos , !pos )))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.SEMICOLON(!pos , !pos )))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.COMMA(!pos , !pos )))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LPAREN(!pos , !pos )))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RPAREN(!pos , !pos )))
fun yyAction53 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.RBRACE(!pos , !pos )))
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (Tokens.LBRACE(!pos , !pos )))
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( Tokens.POSNUMERAL( yytext , !pos , !pos ) )
      end
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( Tokens.RATIONALNUM( yytext , !pos , !pos ) )
      end
fun yyAction57 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; ( Tokens.IDENTIFIER( yytext ,!pos , !pos ))
      end
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction59 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN RATIONAL_PL0; continue()))
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56, yyAction57, yyAction58,
  yyAction59])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of RATIONAL_PL0 => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
        | COMMENT => yygo yyactTable (1, !(yystrm), yyNO_MATCH)
        | INITIAL => yygo yyactTable (2, !(yystrm), yyNO_MATCH)
      (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
