let
  val filename = (hd o CommandLine.arguments) ()
  val file = TextIO.openIn filename
  val str = TextIO.inputAll file
in
  case parse str of
       Ok (json, idx) => print_json json
     | Error (e, idx) => (print e; print " at byte "; (print o Int.toString) idx; print "\n")
end;
