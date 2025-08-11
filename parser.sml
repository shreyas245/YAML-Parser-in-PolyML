(* use "parser.sig"; *)

structure Parser: PARSER =
struct
    val _ = print (" ========= Parser started ========= \n")

    (* Open Token structure to use its types *)
    open Token

    (* YAML AST (Abstract Syntax Tree) data structures *)
    datatype yaml_value = 
        YamlNull
      | YamlBool of bool
      | YamlInt of int
      | YamlFloat of real
      | YamlString of string
      | YamlSequence of yaml_value list
      | YamlMapping of (yaml_value * yaml_value) list

    (* Simple parser state - just position in token stream *)
    type parser_state = {
        tokens: token list,
        position: int
    }

    (* Exception for parser errors *)
    exception ParseError of string

    (* Helper function to create initial parser state *)
    fun initial_parser_state (tokens: token list) : parser_state =
        { tokens = tokens, position = 0 }

    (* Peek at current token without consuming it *)
    fun peek_token (state: parser_state) : token option =
        let
            val tokens = #tokens state
            val pos = #position state
        in
            if pos >= length tokens then NONE
            else SOME (List.nth (tokens, pos))
        end

    (* Consume current token and advance position *)
    fun consume_token (state: parser_state) : token option * parser_state =
        let
            val current_token = peek_token state
            val new_state = { tokens = #tokens state, position = #position state + 1 }
        in
            (current_token, new_state)
        end

    (* Expect a specific token and consume it *)
    fun expect_token (expected: token) (state: parser_state) : parser_state =
        case consume_token state of
            (SOME token, new_state) =>
                if token = expected then new_state
                else raise ParseError ("Expected " ^ token_to_string expected ^ 
                                     " but got " ^ token_to_string token)
          | (NONE, _) => raise ParseError ("Expected " ^ token_to_string expected ^ 
                                         " but reached end of input")

    (* Skip StreamStart if present *)
    fun skip_stream_start (state: parser_state) : parser_state =
        case peek_token state of
            SOME StreamStart => #2 (consume_token state)
          | _ => state

    (* Convert scalar content to appropriate YAML value *)
    fun parse_scalar_value (content: string, style: scalar_style) : yaml_value =
        let
            (* Helper function to count occurrences of a character in a string *)
            fun count_char (s: string, c: char) : int =
                let
                    fun count i acc =
                        if i >= String.size s then acc
                        else if String.sub(s, i) = c then count (i+1) (acc+1)
                        else count (i+1) acc
                in count 0 0 end
        in
            case style of
                Plain =>
                    (* Try to parse as different types for plain scalars *)
                    if content = "null" orelse content = "~" orelse content = "" then
                        YamlNull
                    else if content = "true" orelse content = "on" orelse content = "yes" then
                        YamlBool true
                    else if content = "false" orelse content = "off" orelse content = "no" then
                        YamlBool false
                    else
                        (* Try to parse as number - check for float first (contains .) *)
                        if String.isSubstring "." content then
                            (* If there's more than one dot, treat as string (like IP addresses) *)
                            if count_char(content, #".") > 1 then
                                YamlString content
                            else
                                (case Real.fromString content of
                                     SOME r => YamlFloat r
                                   | NONE => YamlString content)
                        else
                            (* Try integer *)
                            (case Int.fromString content of
                                 SOME i => YamlInt i
                               | NONE => YamlString content)
              | _ => YamlString content (* Quoted strings are always strings *)
        end


    (* Main parsing functions - mutually recursive *)
    (* Parse a single YAML value *)
    fun parse_value (state: parser_state) : yaml_value * parser_state =
        case peek_token state of
            SOME (Scalar (content, style)) =>
                let
                    val (_, new_state) = consume_token state
                    val value = parse_scalar_value (content, style)
                in
                    (value, new_state)
                end
          | SOME FlowSequenceStart => parse_flow_sequence state
          | SOME FlowMappingStart => parse_flow_mapping state
          | SOME BlockEntry => parse_block_sequence state
          | SOME StreamStart => parse_value (skip_stream_start state)
          | SOME DocumentStart => 
                let
                    val (_, new_state) = consume_token state
                in
                    parse_value new_state
                end
          | NONE => raise ParseError "Unexpected end of input"
          (* Add function for handling folding strings  *)
          | SOME token => raise ParseError ("Unexpected token: " ^ token_to_string token)

    (* Parse flow sequence: [item1, item2, ...] *)
    and parse_flow_sequence (state: parser_state) : yaml_value * parser_state =
        let
            val state_after_start = expect_token FlowSequenceStart state
            
            fun parse_items (acc: yaml_value list, current_state: parser_state) 
                : yaml_value list * parser_state =
                case peek_token current_state of
                    SOME FlowSequenceEnd => (rev acc, current_state)
                  | NONE => raise ParseError "Unexpected end of input in flow sequence"
                  | _ =>
                        let
                            val (value, state_after_value) = parse_value current_state
                            val new_acc = value :: acc
                        in
                            case peek_token state_after_value of
                                SOME FlowEntry =>
                                    let
                                        val state_after_comma = expect_token FlowEntry state_after_value
                                    in
                                        parse_items (new_acc, state_after_comma)
                                    end
                              | SOME FlowSequenceEnd => (rev new_acc, state_after_value)
                              | _ => raise ParseError "Expected ',' or ']' in flow sequence"
                        end
            
            val (items, state_before_end) = parse_items ([], state_after_start)
            val final_state = expect_token FlowSequenceEnd state_before_end
        in
            (YamlSequence items, final_state)
        end

    (* Parse flow mapping: {key1: value1, key2: value2, ...} *)
    and parse_flow_mapping (state: parser_state) : yaml_value * parser_state =
        let
            val state_after_start = expect_token FlowMappingStart state
            
            fun parse_pairs (acc: (yaml_value * yaml_value) list, current_state: parser_state) 
                : (yaml_value * yaml_value) list * parser_state =
                case peek_token current_state of
                    SOME FlowMappingEnd => (rev acc, current_state)
                  | NONE => raise ParseError "Unexpected end of input in flow mapping"
                  | _ =>
                        let
                            val (key, state_after_key) = parse_value current_state
                            val state_after_colon = expect_token Value state_after_key
                            val (value, state_after_value) = parse_value state_after_colon
                            val new_acc = (key, value) :: acc
                        in
                            case peek_token state_after_value of
                                SOME FlowEntry =>
                                    let
                                        val state_after_comma = expect_token FlowEntry state_after_value
                                    in
                                        parse_pairs (new_acc, state_after_comma)
                                    end
                              | SOME FlowMappingEnd => (rev new_acc, state_after_value)
                              | _ => raise ParseError "Expected ',' or '}' in flow mapping"
                        end
            
            val (pairs, state_before_end) = parse_pairs ([], state_after_start)
            val final_state = expect_token FlowMappingEnd state_before_end
        in
            (YamlMapping pairs, final_state)
        end

    (* Parse block sequence - using INDENT/DEDENT as explicit scope markers *)
    and parse_block_sequence (state: parser_state) : yaml_value * parser_state =
        let
            fun parse_sequence_items (acc: yaml_value list, current_state: parser_state) 
                : yaml_value list * parser_state =
                case peek_token current_state of
                    SOME BlockEntry =>
                        let
                            val state_after_entry = expect_token BlockEntry current_state
                            (* After BlockEntry, we need to determine what kind of value follows *)
                            val (value, state_after_value) = 
                                case peek_token state_after_entry of
                                    SOME (Scalar _) =>
                                        (* Check if this scalar is followed by Value (colon) *)
                                        (case peek_token_at state_after_entry 1 of
                                            SOME Value =>
                                                (* This is a mapping item in the sequence *)
                                                parse_block_mapping_item state_after_entry
                                        | _ => 
                                                (* This is just a scalar item *)
                                                parse_value state_after_entry)
                                | _ => parse_block_node state_after_entry
                            val new_acc = value :: acc
                        in
                            parse_sequence_items (new_acc, state_after_value)
                        end
                | SOME Dedent => (rev acc, current_state)
                | SOME StreamEnd => (rev acc, current_state)
                | NONE => (rev acc, current_state)
                | _ => (rev acc, current_state)
            
            val (items, final_state) = parse_sequence_items ([], state)
        in
            (YamlSequence items, final_state)
        end

    (* Parse block mapping - using INDENT/DEDENT as explicit scope markers *)
    and parse_block_mapping (state: parser_state) : yaml_value * parser_state =
        let
            fun parse_mapping_pairs (acc: (yaml_value * yaml_value) list, current_state: parser_state) 
                : (yaml_value * yaml_value) list * parser_state =
                case peek_token current_state of
                    SOME (Scalar _) =>
                        (case peek_token_at current_state 1 of
                             SOME Value =>
                                 let
                                     val (key, state_after_key) = parse_value current_state
                                     val state_after_colon = expect_token Value state_after_key
                                     val (value, state_after_value) = parse_mapping_value state_after_colon
                                     val new_acc = (key, value) :: acc
                                 in
                                     parse_mapping_pairs (new_acc, state_after_value)
                                 end
                           | _ => (rev acc, current_state))  (* Not a mapping pair *)
                  | SOME Dedent => (rev acc, current_state)  (* End of this mapping *)
                  | SOME StreamEnd => (rev acc, current_state)
                  | NONE => (rev acc, current_state)
                  | _ => (rev acc, current_state)  (* Not a mapping pair, stop *)
            
            val (pairs, final_state) = parse_mapping_pairs ([], state)
        in
            (YamlMapping pairs, final_state)
        end

    (* Peek at token at specific offset *)
    and peek_token_at (state: parser_state) (offset: int) : token option =
        let
            val tokens = #tokens state
            val pos = #position state + offset
        in
            if pos >= length tokens then NONE
            else SOME (List.nth (tokens, pos))
        end

    (* Parse a block node - handles INDENT/DEDENT scoping *)
    and parse_block_node (state: parser_state) : yaml_value * parser_state =
        case peek_token state of
            SOME Indent =>
                let
                    val state_after_indent = expect_token Indent state
                in
                    (* After INDENT, we're in a new scope *)
                    case peek_token state_after_indent of
                        SOME BlockEntry =>
                            let
                                val (seq_value, state_after_seq) = parse_block_sequence state_after_indent
                                val state_after_dedent = expect_token Dedent state_after_seq
                            in
                                (seq_value, state_after_dedent)
                            end
                      | SOME (Scalar _) =>
                            (case peek_token_at state_after_indent 1 of
                                 SOME Value =>
                                     let
                                         val (map_value, state_after_map) = parse_block_mapping state_after_indent
                                         (* Only expect Dedent if next token is actually Dedent *)
                                         val final_state = 
                                             case peek_token state_after_map of
                                                 SOME Dedent => expect_token Dedent state_after_map
                                               | _ => state_after_map
                                     in
                                         (map_value, final_state)
                                     end
                               | _ => parse_value state_after_indent)
                      | SOME FlowSequenceStart =>
                            (* Handle flow sequences directly *)
                            parse_flow_sequence state_after_indent
                      | SOME FlowMappingStart =>
                            (* Handle flow mappings directly *)
                            parse_flow_mapping state_after_indent
                      | _ => parse_value state_after_indent
                end
          | _ => parse_value state

    (* Parse a mapping that's an item in a block sequence *)
    and parse_block_mapping_item (state: parser_state) : yaml_value * parser_state =
    let
        fun parse_mapping_pairs (acc: (yaml_value * yaml_value) list, current_state: parser_state) 
            : (yaml_value * yaml_value) list * parser_state =
            case peek_token current_state of
                SOME (Scalar _) =>
                    (case peek_token_at current_state 1 of
                        SOME Value =>
                            let
                                val (key, state_after_key) = parse_value current_state
                                val state_after_colon = expect_token Value state_after_key
                                val (value, state_after_value) = parse_mapping_value state_after_colon
                                val new_acc = (key, value) :: acc
                                (* Check for Indent after value *)
                                val (nested_pairs, state_after_nested) =
                                    case peek_token state_after_value of
                                        SOME Indent =>
                                            let
                                                val state_after_indent = expect_token Indent state_after_value
                                                val (nested_map, state_after_map) = parse_block_mapping state_after_indent
                                                val state_after_dedent = expect_token Dedent state_after_map
                                            in
                                                case nested_map of
                                                    YamlMapping pairs => (pairs, state_after_dedent)
                                                  | _ => ([], state_after_dedent)
                                            end
                                      | _ => ([], state_after_value)
                                val merged_acc = List.revAppend(nested_pairs, new_acc)
                            in
                                parse_mapping_pairs (merged_acc, state_after_nested)
                            end
                    | _ => (rev acc, current_state))
            | SOME Dedent => (rev acc, current_state)
            | SOME BlockEntry => (rev acc, current_state)  (* Next sequence item *)
            | SOME StreamEnd => (rev acc, current_state)
            | NONE => (rev acc, current_state)
            | _ => (rev acc, current_state)
        
        val (pairs, final_state) = parse_mapping_pairs ([], state)
    in
        (YamlMapping pairs, final_state)
    end

    (* Parse the value part of a mapping, handling multi-word values *)
    and parse_mapping_value (state: parser_state) : yaml_value * parser_state =
        case peek_token state of
            SOME Indent =>
                (* Indented content - parse as nested structure *)
                let
                    val state_after_indent = expect_token Indent state
                    val (value, state_after_value) = 
                        case peek_token state_after_indent of
                            SOME BlockEntry => 
                                let
                                    val (seq_value, state_after_seq) = parse_block_sequence state_after_indent
                                in
                                    (seq_value, state_after_seq)
                                end
                        | SOME (Scalar _) =>
                                (case peek_token_at state_after_indent 1 of
                                    SOME Value => 
                                        let
                                            val (map_value, state_after_map) = parse_block_mapping state_after_indent
                                        in
                                            (map_value, state_after_map)
                                        end
                                | _ => parse_multi_word_scalar state_after_indent)
                        | SOME FlowSequenceStart =>
                                (* Handle flow sequences directly *)
                                parse_flow_sequence state_after_indent
                        | SOME FlowMappingStart =>
                                (* Handle flow mappings directly *)
                                parse_flow_mapping state_after_indent
                        | _ => parse_value state_after_indent
                    val state_after_dedent = 
                        case peek_token state_after_value of
                            SOME Dedent => expect_token Dedent state_after_value
                        | _ => state_after_value
                in
                    (value, state_after_dedent)
                end
        | SOME FlowSequenceStart => parse_flow_sequence state
        | SOME FlowMappingStart => parse_flow_mapping state
        | _ => parse_multi_word_scalar state

    (* Parse multi-word scalar values (like "not mentioned") *)
    and parse_multi_word_scalar (state: parser_state) : yaml_value * parser_state =
        let
            fun collect_scalar_words (acc: string list, current_state: parser_state) 
                : string list * parser_state =
                case peek_token current_state of
                    SOME (Scalar (content, style)) =>
                        let
                            val (_, state_after_scalar) = consume_token current_state
                        in
                            (* Check if next token continues the value or starts new structure *)
                            case peek_token state_after_scalar of
                                SOME Dedent => (content :: acc, state_after_scalar)
                            | SOME BlockEntry => (content :: acc, state_after_scalar)
                            | SOME (Scalar _) =>
                                    (* Check if the next scalar is a key (followed by Value) *)
                                    (case peek_token_at state_after_scalar 1 of
                                        SOME Value => (content :: acc, state_after_scalar)
                                    | _ => collect_scalar_words (content :: acc, state_after_scalar))
                            | SOME StreamEnd => (content :: acc, state_after_scalar)
                            | NONE => (content :: acc, state_after_scalar)
                            | _ => collect_scalar_words (content :: acc, state_after_scalar)
                        end
                | _ => (acc, current_state)
            
            val (words, final_state) = collect_scalar_words ([], state)
            val combined_value = String.concatWith " " (rev words)
            val yaml_value = parse_scalar_value (combined_value, Plain)
        in
            (yaml_value, final_state)
        end

    (* Determine the document structure and parse accordingly *)
    and parse_document (state: parser_state) : yaml_value * parser_state =
        let
            val state_start = skip_stream_start state
            (* Skip DocumentStart if present *)
            val state_after_doc_start = 
                case peek_token state_start of
                    SOME DocumentStart =>
                        let
                            val (_, new_state) = consume_token state_start
                        in
                            new_state
                        end
                  | _ => state_start
        in
            case peek_token state_after_doc_start of
                SOME BlockEntry => 
                    (* This is a block sequence at root level *)
                    parse_block_sequence state_after_doc_start
            | SOME (Scalar _) =>
                    (case peek_token_at state_after_doc_start 1 of
                        SOME Value => 
                            (* This is a block mapping at root level *)
                            parse_block_mapping state_after_doc_start
                    | _ => parse_value state_after_doc_start)
            | _ => parse_value state_after_doc_start
        end
    fun parse (tokens: token list) : yaml_value =
        let
            val initial_state = initial_parser_state tokens
            val (result, final_state) = parse_document initial_state
        in
            result
        end

    (* Convenience function to parse from string *)
    fun parse_string (input: string) : yaml_value =
        let
            val tokens = tokenize input
        in
            parse tokens
        end

    (* Enhanced pretty printer for YAML values *)
    fun yaml_to_string_pretty (value: yaml_value) : string =
        let
            fun indent_lines (indent_level: int) (s: string) : string =
                let
                    val indent_str = String.concat (List.tabulate (indent_level * 2, fn _ => " "))
                    val lines = String.tokens (fn c => c = #"\n") s
                    val indented_lines = map (fn line => 
                        if line = "" then line else indent_str ^ line) lines
                in
                    String.concatWith "\n" indented_lines
                end
            
            fun to_string_helper (v: yaml_value) (indent: int) : string =
                case v of
                    YamlNull => "null"
                  | YamlBool true => "true"
                  | YamlBool false => "false"
                  | YamlInt i => Int.toString i
                  | YamlFloat r => Real.toString r
                  | YamlString s => "\"" ^ s ^ "\""
                  | YamlSequence items =>
                        if null items then "[]"
                        else
                            let
                                val item_strings = map (fn item => 
                                    "- " ^ to_string_helper item (indent + 1)) items
                            in
                                String.concatWith "\n" item_strings
                            end
                  | YamlMapping pairs =>
                        if null pairs then "{}"
                        else
                            let
                                fun pair_to_string (key, value) = 
                                    let
                                        val key_str = to_string_helper key 0
                                        val value_str = case value of
                                            YamlMapping _ => "\n" ^ indent_lines (indent + 1) (to_string_helper value (indent + 1))
                                          | YamlSequence _ => "\n" ^ indent_lines (indent + 1) (to_string_helper value (indent + 1))
                                          | _ => " " ^ to_string_helper value 0
                                    in
                                        key_str ^ ":" ^ value_str
                                    end
                            in
                                String.concatWith "\n" (map pair_to_string pairs)
                            end
        in
            to_string_helper value 0
        end

    (* Original simple pretty printer *)
    fun yaml_to_string (value: yaml_value) : string =
        case value of
            YamlNull => "null"
          | YamlBool true => "true"
          | YamlBool false => "false"
          | YamlInt i => Int.toString i
          | YamlFloat r => Real.toString r
          | YamlString s => "\"" ^ s ^ "\""
          | YamlSequence items =>
                "[" ^ String.concatWith ", " (map yaml_to_string items) ^ "]"
          | YamlMapping pairs =>
                let
                    fun pair_to_string (key, value) = 
                        yaml_to_string key ^ ": " ^ yaml_to_string value
                in
                    "{" ^ String.concatWith ", " (map pair_to_string pairs) ^ "}"
                end


    (* Recursively print all values for a given key in any yaml_value structure *)
    fun print_key_recursive value key =
        let
            fun search (YamlMapping pairs) =
                    let
                        fun find [] = ()
                        | find ((YamlString k, v)::rest) =
                                (if k = key then print (yaml_to_string_pretty v ^ "\n") else ();
                                search v;
                                find rest)
                        | find ((_, v)::rest) = (search v; find rest)
                    in
                        find pairs
                    end
            | search (YamlSequence items) = List.app search items
            | search _ = ()
        in
            search value
        end

    (* Utility functions for working with YAML mappings *)
    
    (* Check if a key exists in a YAML mapping *)
    fun has_key (YamlMapping pairs) key =
            List.exists (fn (YamlString k, _) => k = key | _ => false) pairs
      | has_key _ _ = false

    (* Get the value associated with a key in a YAML mapping *)
    fun get_value (YamlMapping pairs) key =
            (case List.find (fn (YamlString k, _) => k = key | _ => false) pairs of
                SOME (_, value) => SOME value
              | NONE => NONE)
      | get_value _ _ = NONE

    (* Get a string value for a key, returns NONE if key doesn't exist or value isn't a string *)
    fun get_string_value yaml key =
        case get_value yaml key of
            SOME (YamlString s) => SOME s
          | _ => NONE

    (* Get a sequence value for a key, returns NONE if key doesn't exist or value isn't a sequence *)
    fun get_sequence_value yaml key =
        case get_value yaml key of
            SOME (YamlSequence items) => SOME items
          | _ => NONE

end