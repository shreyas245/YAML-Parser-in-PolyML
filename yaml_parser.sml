(* use "yaml_parser.sig";
use "token.sml";
use "parser.sml"; *)

structure YamlParser: YAML_PARSER =
struct
  (* Re-export the yaml_value datatype from Parser *)
  datatype yaml_value = 
      YamlNull
    | YamlBool of bool
    | YamlInt of int
    | YamlFloat of real
    | YamlString of string
    | YamlSequence of yaml_value list
    | YamlMapping of (yaml_value * yaml_value) list

  (* Exception for YAML parsing errors *)
  exception YamlParseError of string

  (* Convert Parser.yaml_value to YamlParser.yaml_value *)
  fun convertValue (Parser.YamlNull) = YamlNull
    | convertValue (Parser.YamlBool b) = YamlBool b
    | convertValue (Parser.YamlInt i) = YamlInt i
    | convertValue (Parser.YamlFloat f) = YamlFloat f
    | convertValue (Parser.YamlString s) = YamlString s
    | convertValue (Parser.YamlSequence lst) = YamlSequence (map convertValue lst)
    | convertValue (Parser.YamlMapping pairs) = 
        YamlMapping (map (fn (k, v) => (convertValue k, convertValue v)) pairs)

  (* Helper function to read file content *)
  fun readFileContent (filename: string) : string =
      let
          val infile = TextIO.openIn filename
          val content = TextIO.inputAll infile
          val _ = TextIO.closeIn infile
      in
          content
      end
      handle Io => raise YamlParseError ("Could not read file: " ^ filename)

  (* High-level parsing functions *)
  fun parseFile (filename: string) : yaml_value =
      let
          val content = readFileContent filename
          val tokens = Token.tokenize content
          val result = Parser.parse tokens
      in
          convertValue result
      end
      handle Parser.ParseError msg => raise YamlParseError msg

  fun parseString (content: string) : yaml_value =
      let
          val tokens = Token.tokenize content
          val result = Parser.parse tokens
      in
          convertValue result
      end
      handle Parser.ParseError msg => raise YamlParseError msg

  (* Search and extraction functions *)
  fun findKey (YamlMapping pairs) key =
      (case List.find (fn (YamlString k, _) => k = key | _ => false) pairs of
          SOME (_, value) => SOME value
        | NONE => NONE)
    | findKey _ _ = NONE

  fun findKeyRecursive (value: yaml_value) (key: string) : yaml_value list =
      let
          fun search (YamlMapping pairs) =
              let
                  fun searchPairs [] acc = acc
                    | searchPairs ((YamlString k, v)::rest) acc =
                        let
                            val newAcc = if k = key then v :: acc else acc
                            val recursiveResults = search v
                        in
                            searchPairs rest (recursiveResults @ newAcc)
                        end
                    | searchPairs ((_, v)::rest) acc =
                        let
                            val recursiveResults = search v
                        in
                            searchPairs rest (recursiveResults @ acc)
                        end
              in
                  searchPairs pairs []
              end
            | search (YamlSequence items) =
                List.foldl (fn (item, acc) => search item @ acc) [] items
            | search _ = []
      in
          search value
      end

  (* Pretty printing *)
  fun toString (YamlNull) = "null"
    | toString (YamlBool true) = "true"
    | toString (YamlBool false) = "false"
    | toString (YamlInt i) = Int.toString i
    | toString (YamlFloat f) = Real.toString f
    | toString (YamlString s) = "\"" ^ s ^ "\""
    | toString (YamlSequence lst) = 
        "[" ^ String.concatWith ", " (map toString lst) ^ "]"
    | toString (YamlMapping pairs) =
        let
            fun pairToString (k, v) = toString k ^ ": " ^ toString v
        in
            "{" ^ String.concatWith ", " (map pairToString pairs) ^ "}"
        end

  fun toPrettyString value =
      let
          fun indent level = String.concat (List.tabulate (level * 2, fn _ => " "))
          
          fun prettyPrint (YamlNull, level) = "null"
            | prettyPrint (YamlBool true, level) = "true"
            | prettyPrint (YamlBool false, level) = "false"
            | prettyPrint (YamlInt i, level) = Int.toString i
            | prettyPrint (YamlFloat f, level) = Real.toString f
            | prettyPrint (YamlString s, level) = s
            | prettyPrint (YamlSequence [], level) = "[]"
            | prettyPrint (YamlSequence lst, level) =
                let
                    fun itemToString item = "\n" ^ indent (level + 1) ^ "- " ^ prettyPrint (item, level + 1)
                in
                    String.concat (map itemToString lst)
                end
            | prettyPrint (YamlMapping [], level) = "{}"
            | prettyPrint (YamlMapping pairs, level) =
                let
                    fun pairToString (k, v) = 
                        "\n" ^ indent (level + 1) ^ prettyPrint (k, level + 1) ^ ": " ^ prettyPrint (v, level + 1)
                in
                    String.concat (map pairToString pairs)
                end
      in
          prettyPrint (value, 0)
      end

  fun printKeyValue (value: yaml_value) (key: string) : unit =
      let
          val results = findKeyRecursive value key
          fun printResult (v: yaml_value) =
              print (toPrettyString v ^ "\n")
      in
          List.app printResult results
      end

  (* Type-safe extraction functions *)
  fun asString (YamlString s) = SOME s
    | asString _ = NONE

  fun asInt (YamlInt i) = SOME i
    | asInt _ = NONE

  fun asBool (YamlBool b) = SOME b
    | asBool _ = NONE

  fun asFloat (YamlFloat f) = SOME f
    | asFloat _ = NONE

  fun asSequence (YamlSequence lst) = SOME lst
    | asSequence _ = NONE

  fun asMapping (YamlMapping pairs) = SOME pairs
    | asMapping _ = NONE

  (* Pretty printing *)
  fun toString (YamlNull) = "null"
    | toString (YamlBool true) = "true"
    | toString (YamlBool false) = "false"
    | toString (YamlInt i) = Int.toString i
    | toString (YamlFloat f) = Real.toString f
    | toString (YamlString s) = "\"" ^ s ^ "\""
    | toString (YamlSequence lst) = 
        "[" ^ String.concatWith ", " (map toString lst) ^ "]"
    | toString (YamlMapping pairs) =
        let
            fun pairToString (k, v) = toString k ^ ": " ^ toString v
        in
            "{" ^ String.concatWith ", " (map pairToString pairs) ^ "}"
        end

  fun toPrettyString value =
      let
          fun indent level = String.concat (List.tabulate (level * 2, fn _ => " "))
          
          fun prettyPrint (YamlNull, level) = "null"
            | prettyPrint (YamlBool true, level) = "true"
            | prettyPrint (YamlBool false, level) = "false"
            | prettyPrint (YamlInt i, level) = Int.toString i
            | prettyPrint (YamlFloat f, level) = Real.toString f
            | prettyPrint (YamlString s, level) = s
            | prettyPrint (YamlSequence [], level) = "[]"
            | prettyPrint (YamlSequence lst, level) =
                let
                    fun itemToString item = "\n" ^ indent (level + 1) ^ "- " ^ prettyPrint (item, level + 1)
                in
                    String.concat (map itemToString lst)
                end
            | prettyPrint (YamlMapping [], level) = "{}"
            | prettyPrint (YamlMapping pairs, level) =
                let
                    fun pairToString (k, v) = 
                        "\n" ^ indent (level + 1) ^ prettyPrint (k, level + 1) ^ ": " ^ prettyPrint (v, level + 1)
                in
                    String.concat (map pairToString pairs)
                end
      in
          prettyPrint (value, 0)
      end

  fun printKeyValue (value: yaml_value) (key: string) : unit =
      let
          val results = findKeyRecursive value key
          fun printResult (v: yaml_value) =
              print (toPrettyString v ^ "\n")
      in
          List.app printResult results
      end

  (* Helper functions for common patterns *)
  fun getStringValue yaml key =
      case findKey yaml key of
          SOME value => asString value
        | NONE => NONE

  fun getIntValue yaml key =
      case findKey yaml key of
          SOME value => asInt value
        | NONE => NONE

  fun getBoolValue yaml key =
      case findKey yaml key of
          SOME value => asBool value
        | NONE => NONE

  fun getSequenceValue yaml key =
      case findKey yaml key of
          SOME value => asSequence value
        | NONE => NONE

  (* Extract string list from a sequence *)
  fun extractStringList (YamlSequence lst) =
      let
          fun extractStrings [] acc = SOME (rev acc)
            | extractStrings (YamlString s :: rest) acc = extractStrings rest (s :: acc)
            | extractStrings _ _ = NONE
      in
          extractStrings lst []
      end
    | extractStringList _ = NONE

  (* Check if key exists *)
  fun hasKey (YamlMapping pairs) key =
      List.exists (fn (YamlString k, _) => k = key | _ => false) pairs
    | hasKey _ _ = false

end
