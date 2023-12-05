(* while_ast.sml *)
structure My=
    struct
    exception MyError;
    fun compile (fileName) =
    let
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
            then ""
            else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
        (msg,line,col) =>
        print (fileName^"["^Int.toString line^":"
        ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = MyParser.parse
        (15,
        (MyParser.makeLexer grab ),
        printError,
        ()) 
        handle MyParser.ParseError => raise MyError ; 
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in 
        tree    
    end

fun writeListToFile (filename : string, lst : string list) =
  let
    val outStream = TextIO.openOut filename
    fun writeList (lst : string list) =
      case lst of
        [] => ()
      | x::xs => (TextIO.output (outStream, ( x ^ "\n")); writeList xs)
  in
    writeList lst;
    TextIO.closeOut outStream
  end


    fun interpret(filename,fileout)=
    let 
    open DataTypes
        val parsed_ast = compile(filename)
        val stringop = letsgo(parsed_ast)
        val _ = writeListToFile(fileout,!stringop)        
    in
        ()
    end

    
end;
