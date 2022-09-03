let
  val print_flag :: filename :: [] = CommandLine.arguments ()
  val should_print = print_flag = "-p"
  val file = TextIO.openIn filename
  val str = TextIO.inputAll file
in
  case parse str of
       Ok (json, idx) =>
         if should_print then print_json json
         else print "Ok\n"
     | Error (e, idx) =>
         (print e; print " at byte "; (print o Int.toString) idx; print "\n";
         OS.Process.exit OS.Process.failure)
end;
