signature TOKEN =
sig
  datatype scalar = SNull | SBool of bool | SFloat of real | SInt of int | SStr of string | SUnknown
  datatype scalar_style = Plain | SingleQuoted | DoubleQuoted | Literal | Fold
  datatype token =
      StreamStart | StreamEnd | DocumentStart | DocumentEnd
    | BlockSequenceStart | BlockMappingStart | BlockEnd
    | FlowSequenceStart | FlowSequenceEnd
    | FlowMappingStart | FlowMappingEnd
    | Key | Value | BlockEntry | FlowEntry
    | Scalar of string * scalar_style
    | Comment of string
    | Tag of string
    | Indent 
    | Dedent

  type tokenizer_state

  val initialState : string -> tokenizer_state
  val peek : tokenizer_state -> char option
  val advance : tokenizer_state -> tokenizer_state
  val consume_while : (char -> bool) -> tokenizer_state -> string * tokenizer_state
  val count_indent : tokenizer_state -> int * tokenizer_state
  val skip_whitespace_preserve_newlines : tokenizer_state -> tokenizer_state
  val skip_whitespace : tokenizer_state -> tokenizer_state
  val manage_indent : tokenizer_state -> tokenizer_state
  val parse_plain_scalar : tokenizer_state -> string * tokenizer_state
  val parse_single_quoted_scalar : tokenizer_state -> string * tokenizer_state
  val parse_double_quoted_scalar : tokenizer_state -> string * tokenizer_state
  val is_at_end : tokenizer_state -> bool
  val next_token : tokenizer_state -> token option * tokenizer_state
  val tokenize : string -> token list
  val make_tokenizer : string -> tokenizer_state
  val token_to_string : token -> string

end