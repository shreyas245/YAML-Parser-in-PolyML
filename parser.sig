signature PARSER =
sig
  exception ParseError of string

  datatype yaml_value =
      YamlNull
    | YamlBool of bool
    | YamlInt of int
    | YamlFloat of real
    | YamlString of string
    | YamlSequence of yaml_value list
    | YamlMapping of (yaml_value * yaml_value) list

  type parser_state

  val initial_parser_state : Token.token list -> parser_state
  val peek_token : parser_state -> Token.token option
  val consume_token : parser_state -> Token.token option * parser_state
  val expect_token : Token.token -> parser_state -> parser_state
  val skip_stream_start : parser_state -> parser_state
  val parse_scalar_value : string * Token.scalar_style -> yaml_value
  val parse_value : parser_state -> yaml_value * parser_state
  val parse_flow_sequence : parser_state -> yaml_value * parser_state
  val parse_flow_mapping : parser_state -> yaml_value * parser_state
  val parse_block_sequence : parser_state -> yaml_value * parser_state
  val parse_block_mapping : parser_state -> yaml_value * parser_state
  val peek_token_at : parser_state -> int -> Token.token option
  val parse_block_node : parser_state -> yaml_value * parser_state
  val parse_document : parser_state -> yaml_value * parser_state
  val parse : Token.token list -> yaml_value
  val parse_string : string -> yaml_value
  val yaml_to_string_pretty : yaml_value -> string
  val yaml_to_string : yaml_value -> string
  val print_key_recursive: yaml_value -> string -> unit
  
  (* Utility functions for working with YAML mappings *)
  val has_key : yaml_value -> string -> bool
  val get_value : yaml_value -> string -> yaml_value option
  val get_string_value : yaml_value -> string -> string option
  val get_sequence_value : yaml_value -> string -> yaml_value list option
end