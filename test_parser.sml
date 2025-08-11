(* test_parser.sml *)
use "yaml_parser.sml";
structure TestParser =
struct
  open YamlParser

  val filename = case CommandLine.arguments() of
                   [] => "config.yaml"
                 | f :: _ => f

  fun main () =
    (print ("Parsing file: " ^ filename ^ "\n");
     let
       val yaml = parseFile filename
     in
       print ("Parsed YAML:\n" ^ toString yaml ^ "\n")
     end
     handle YamlParseError msg => print ("YAML Parse Error: " ^ msg ^ "\n"))
end

val _ = TestParser.main ()