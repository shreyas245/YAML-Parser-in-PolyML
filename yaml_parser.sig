signature YAML_PARSER =
sig
  (* Re-export the yaml_value datatype *)
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

  (* High-level parsing functions *)
  val parseFile : string -> yaml_value
  val parseString : string -> yaml_value

  (* Search and extraction functions *)
  val findKey : yaml_value -> string -> yaml_value option
  val findKeyRecursive : yaml_value -> string -> yaml_value list
  val printKeyValue : yaml_value -> string -> unit
  
  (* Type-safe extraction functions *)
  val asString : yaml_value -> string option
  val asInt : yaml_value -> int option
  val asBool : yaml_value -> bool option
  val asFloat : yaml_value -> real option
  val asSequence : yaml_value -> yaml_value list option
  val asMapping : yaml_value -> (yaml_value * yaml_value) list option
  
  (* Helper functions for common patterns *)
  val getStringValue : yaml_value -> string -> string option
  val getIntValue : yaml_value -> string -> int option
  val getBoolValue : yaml_value -> string -> bool option
  val getSequenceValue : yaml_value -> string -> yaml_value list option
  
  (* Pretty printing *)
  val toString : yaml_value -> string
  val toPrettyString : yaml_value -> string
  
  (* Extract string list from a sequence (useful for usernames, etc.) *)
  val extractStringList : yaml_value -> string list option
  
  (* Check if key exists *)
  val hasKey : yaml_value -> string -> bool
end
