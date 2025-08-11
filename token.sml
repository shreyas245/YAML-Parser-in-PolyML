(* use "util.sml"; *)
(* use "token.sig"; *)

structure Token: TOKEN =
struct

  datatype scalar = SNull | SBool of bool | SFloat of real | SInt of int | SStr of string | SUnknown

  datatype scalar_style = Plain | SingleQuoted | DoubleQuoted | Literal | Fold

  datatype token = StreamStart | StreamEnd | DocumentStart | DocumentEnd
    | BlockSequenceStart | BlockMappingStart | BlockEnd
    | FlowSequenceStart | FlowSequenceEnd
    | FlowMappingStart | FlowMappingEnd
    | Key | Value | BlockEntry | FlowEntry
    | Scalar of string * scalar_style
    | Comment of string
    | Tag of string
    | Indent 
    | Dedent

    type tokenizer_state = {
        input: char list,
        position: int * int,
        indent_stack: int list,
        flow_level: int,
        pending_tokens: token list,  (* Queue for tokens that need to be emitted *)
        at_line_start: bool          (* Track if we're at the beginning of a line *)
    }

    (* Helper function to create the initial tokenizer state from an input string. *)
    fun initialState (s: string) : tokenizer_state =
        { input = String.explode s, position = (1, 1), indent_stack = [0], flow_level = 0, 
          pending_tokens = [], at_line_start = true }

    (* Peeks at the next character in the input without consuming it. *)
    fun peek (state: tokenizer_state) : char option =
        case #input state of
            [] => NONE
          | c :: _ => SOME c

    (* Consumes the next character from the input and updates the state (position). *)
    fun advance (state: tokenizer_state) : tokenizer_state =
        case #input state of
            [] => state (* No more input to advance *)
          | c :: rest =>
                let
                    val (line, col) = #position state
                in
                    if c = #"\n" then
                        (* If newline, reset column and increment line, mark as line start *)
                        { input = rest, position = (line + 1, 1), 
                          indent_stack = #indent_stack state, flow_level = #flow_level state,
                          pending_tokens = #pending_tokens state, at_line_start = true }
                    else
                        (* Otherwise, just increment column, no longer at line start *)
                        { input = rest, position = (line, col + 1), 
                          indent_stack = #indent_stack state, flow_level = #flow_level state,
                          pending_tokens = #pending_tokens state, at_line_start = false }
                end
       
    (* Consumes characters from the input as long as a given predicate is true.
       Returns the consumed string and the new tokenizer state. *)
    fun consume_while (pred: char -> bool) (state: tokenizer_state) : string * tokenizer_state =
        let
            (* Recursive helper to build the string of consumed characters. *)
            fun loop (current_chars: char list, current_state: tokenizer_state) : string * tokenizer_state =
                case peek current_state of
                    SOME c =>
                        if pred c then
                            let
                                val next_state = advance current_state
                            in
                                loop (c :: current_chars, next_state) (* Prepend char and continue *)
                            end
                        else
                            (String.implode (rev current_chars), current_state) (* Predicate failed, return collected chars *)
                  | NONE => (String.implode (rev current_chars), current_state) (* End of input *)
        in
            loop ([], state) (* Start with an empty list of characters to collect *)
        end

    (* Count leading spaces/tabs at the beginning of a line *)
    fun count_indent (state: tokenizer_state) : int * tokenizer_state =
        let
            fun loop (count: int, current_state: tokenizer_state) : int * tokenizer_state =
                case peek current_state of
                    SOME #" " => loop (count + 1, advance current_state)
                  | SOME #"\t" => loop (count + 8, advance current_state) (* Tab = 8 spaces *)
                  | _ => (count, current_state)
        in
            loop (0, state)
        end

    (* Skips all leading whitespace characters in the current state, but preserves line breaks for indent processing *)
    fun skip_whitespace_preserve_newlines (state: tokenizer_state) : tokenizer_state =
        let
            fun loop (current_state: tokenizer_state) : tokenizer_state =
                case peek current_state of
                    SOME c =>
                        if c = #" " orelse c = #"\t" then
                            loop (advance current_state)
                        else
                            current_state
                  | NONE => current_state
        in
            loop state
        end

    (* Regular skip whitespace for when we don't care about indentation *)
    fun skip_whitespace (state: tokenizer_state) : tokenizer_state =
        #2 (consume_while Util.isSpace state)

    (* Manage indentation and generate Indent/Dedent tokens *)
    fun manage_indent (state: tokenizer_state) : tokenizer_state =
        if #flow_level state > 0 then
            (* In flow context, ignore indentation *)
            state
        else if not (#at_line_start state) then
            (* Not at line start, no indent processing needed *)
            state
        else
            let
                val (current_indent, state_after_indent) = count_indent state
                val current_stack = #indent_stack state
                val current_level = case current_stack of
                    [] => 0
                  | top :: _ => top
                
                fun generate_dedents (target_level: int, stack: int list, tokens: token list) : int list * token list =
                    case stack of
                        [] => ([], tokens)
                      | top :: rest =>
                            if top > target_level then
                                generate_dedents (target_level, rest, Dedent :: tokens)
                            else
                                (stack, tokens)
            in
                if current_indent > current_level then
                    (* Increase in indentation - emit Indent token *)
                    { input = #input state_after_indent,
                      position = #position state_after_indent,
                      indent_stack = current_indent :: current_stack,
                      flow_level = #flow_level state,
                      pending_tokens = Indent :: (#pending_tokens state),
                      at_line_start = false }
                else if current_indent < current_level then
                    (* Decrease in indentation - emit Dedent tokens *)
                    let
                        val (new_stack, dedent_tokens) = generate_dedents (current_indent, current_stack, [])
                    in
                        { input = #input state_after_indent,
                          position = #position state_after_indent,
                          indent_stack = new_stack,
                          flow_level = #flow_level state,
                          pending_tokens = dedent_tokens @ (#pending_tokens state),
                          at_line_start = false }
                    end
                else
                    (* Same indentation level - no tokens needed *)
                    { input = #input state_after_indent,
                      position = #position state_after_indent,
                      indent_stack = current_stack,
                      flow_level = #flow_level state,
                      pending_tokens = #pending_tokens state,
                      at_line_start = false }
            end

    (* Parses a "plain" scalar, which for this simple example means a sequence of
       alphanumeric characters or underscores. Real YAML plain scalars are more complex. *)
    fun parse_plain_scalar (state: tokenizer_state) : string * tokenizer_state =
        let
            fun is_plain_scalar_char c = not (c = #":" orelse c = #"#" orelse c = #"[" orelse c = #"]" orelse
                 c = #"{" orelse c = #"}" orelse c = #"," orelse c = #"\n" orelse c = #"\r")
            val (scalar_str, next_state) = consume_while is_plain_scalar_char state
            in
            (scalar_str, next_state)
        end

    (* Parses a single-quoted scalar *)
    fun parse_single_quoted_scalar (state: tokenizer_state) : string * tokenizer_state =
        let
            (* Called the advance function once to skip the starting quote *)
            val state_after_quote = advance state
            
            fun loop (chars: char list, current_state: tokenizer_state) : string * tokenizer_state =
                case peek current_state of
                    NONE => (String.implode (rev chars), current_state) (* Unclosed quote *)
                  | SOME #"'" => 
                        (* Found closing quote, skip it and return *)
                        (String.implode (rev chars), advance current_state)
                  | SOME c =>
                        let
                            val next_state = advance current_state
                        in
                            loop (c :: chars, next_state)
                        end
        in
            loop ([], state_after_quote)
        end

    (* Parses a double-quoted scalar *) 
    fun parse_double_quoted_scalar (state: tokenizer_state) : string * tokenizer_state =
        let
            (* Skip the opening quote *)
            val state_after_quote = advance state
            
            fun loop (chars: char list, current_state: tokenizer_state) : string * tokenizer_state =
                case peek current_state of
                    NONE => (String.implode (rev chars), current_state) (* Unclosed quote *)
                  | SOME #"\"" => 
                        (* Found closing quote, skip it and return *)
                        (String.implode (rev chars), advance current_state)
                  | SOME #"\\" =>
                        (* Handle escape sequences - simplified for this example *)
                        let
                            val state_after_backslash = advance current_state
                        in
                            case peek state_after_backslash of
                                SOME escaped_char =>
                                    let
                                        val final_state = advance state_after_backslash
                                        val actual_char = case escaped_char of
                                            #"n" => #"\n"
                                          | #"t" => #"\t"
                                          | #"r" => #"\r"
                                          | #"\\" => #"\\"
                                          | #"\"" => #"\""
                                          | c => c (* For simplicity, just use the character *)
                                    in
                                        loop (actual_char :: chars, final_state)
                                    end
                              | NONE => (String.implode (rev chars), state_after_backslash)
                        end
                  | SOME c =>
                        let
                            val next_state = advance current_state
                        in
                            loop (c :: chars, next_state)
                        end
        in
            loop ([], state_after_quote)
        end

    (* Helper function to check if we're at the end of input *)
    fun is_at_end (state: tokenizer_state) : bool =
        case #input state of
            [] => true
          | _ => false

    (* Get the next token from pending tokens or parse a new one *)
    fun next_token (state: tokenizer_state) : token option * tokenizer_state =
        (* First check if we have pending tokens *)
        case #pending_tokens state of
            token :: rest =>
                let
                    val new_state = { input = #input state,
                                    position = #position state,
                                    indent_stack = #indent_stack state,
                                    flow_level = #flow_level state,
                                    pending_tokens = rest,
                                    at_line_start = #at_line_start state }
                in
                    (SOME token, new_state)
                end
          | [] =>
                if is_at_end state then
                    (* Generate final dedents before StreamEnd *)
                    let
                        val final_dedents = List.length (#indent_stack state) - 1
                        fun make_dedents 0 = []
                          | make_dedents n = Dedent :: make_dedents (n - 1)
                    in
                        if final_dedents > 0 then
                            let
                                val dedent_tokens = make_dedents final_dedents
                                val new_state = { input = #input state,
                                                position = #position state,
                                                indent_stack = [0],
                                                flow_level = #flow_level state,
                                                pending_tokens = dedent_tokens,
                                                at_line_start = #at_line_start state }
                            in
                                next_token new_state
                            end
                        else
                            (NONE, state)
                    end
                else
                    let
                        (* Skip newlines and handle indentation *)
                        fun skip_newlines (current_state: tokenizer_state) : tokenizer_state =
                            case peek current_state of
                                SOME #"\n" => 
                                    let
                                        val after_newline = advance current_state
                                        val after_newline_marked = { input = #input after_newline,
                                                                   position = #position after_newline,
                                                                   indent_stack = #indent_stack after_newline,
                                                                   flow_level = #flow_level after_newline,
                                                                   pending_tokens = #pending_tokens after_newline,
                                                                   at_line_start = true }
                                    in
                                        skip_newlines after_newline_marked
                                    end
                              | _ => current_state
                        
                        val state_after_newlines = skip_newlines state
                        val state_after_indent = manage_indent state_after_newlines
                    in
                        (* Check if we have pending tokens after indent processing *)
                        case #pending_tokens state_after_indent of
                            token :: rest =>
                                let
                                    val new_state = { input = #input state_after_indent,
                                                    position = #position state_after_indent,
                                                    indent_stack = #indent_stack state_after_indent,
                                                    flow_level = #flow_level state_after_indent,
                                                    pending_tokens = rest,
                                                    at_line_start = #at_line_start state_after_indent }
                                in
                                    (SOME token, new_state)
                                end
                          | [] =>
                                (* No pending tokens, parse normally *)
                                let
                                    val state_no_ws = skip_whitespace_preserve_newlines state_after_indent
                                in
                                    if is_at_end state_no_ws then
                                        (NONE, state_no_ws)
                                    else
                                        case peek state_no_ws of
                                            NONE => (NONE, state_no_ws)
                                          | SOME #"-" =>
                                                let
                                                    val next_state = advance state_no_ws
                                                in
                                                    case peek next_state of
                                                        SOME #"-" =>
                                                            let
                                                                val state_after_second = advance next_state
                                                            in
                                                                case peek state_after_second of
                                                                    SOME #"-" =>
                                                                        (* Document separator --- *)
                                                                        let
                                                                            val final_state = advance state_after_second
                                                                        in
                                                                            (SOME DocumentStart, final_state)
                                                                        end
                                                                  | _ => (SOME BlockEntry, next_state)
                                                            end
                                                    | _ => (SOME BlockEntry, next_state)
                                                end
                                          | SOME #":" =>
                                                let
                                                    val next_state = advance state_no_ws
                                                in
                                                    (SOME Value, next_state)
                                                end
                                          | SOME #"[" =>
                                                let
                                                    val next_state = advance state_no_ws
                                                    val updated_state = { input = #input next_state, 
                                                                        position = #position next_state, 
                                                                        indent_stack = #indent_stack next_state, 
                                                                        flow_level = #flow_level next_state + 1,
                                                                        pending_tokens = #pending_tokens next_state,
                                                                        at_line_start = #at_line_start next_state }
                                                in
                                                    (SOME FlowSequenceStart, updated_state)
                                                end
                                          | SOME #"]" =>
                                                let
                                                    val next_state = advance state_no_ws
                                                    val updated_state = { input = #input next_state, 
                                                                        position = #position next_state, 
                                                                        indent_stack = #indent_stack next_state, 
                                                                        flow_level = #flow_level next_state - 1,
                                                                        pending_tokens = #pending_tokens next_state,
                                                                        at_line_start = #at_line_start next_state }
                                                in
                                                    (SOME FlowSequenceEnd, updated_state)
                                                end
                                          | SOME #"{" =>
                                                let
                                                    val next_state = advance state_no_ws
                                                    val updated_state = { input = #input next_state, 
                                                                        position = #position next_state, 
                                                                        indent_stack = #indent_stack next_state, 
                                                                        flow_level = #flow_level next_state + 1,
                                                                        pending_tokens = #pending_tokens next_state,
                                                                        at_line_start = #at_line_start next_state }
                                                in
                                                    (SOME FlowMappingStart, updated_state)
                                                end
                                          | SOME #"}" =>
                                                let
                                                    val next_state = advance state_no_ws
                                                    val updated_state = { input = #input next_state, 
                                                                        position = #position next_state, 
                                                                        indent_stack = #indent_stack next_state, 
                                                                        flow_level = #flow_level next_state - 1,
                                                                        pending_tokens = #pending_tokens next_state,
                                                                        at_line_start = #at_line_start next_state }
                                                in
                                                    (SOME FlowMappingEnd, updated_state)
                                                end
                                          | SOME #"," =>
                                                let
                                                    val next_state = advance state_no_ws
                                                in
                                                    (SOME FlowEntry, next_state)
                                                end
                                          | SOME #"'" =>
                                                let
                                                    val (scalar_content, next_state) = parse_single_quoted_scalar state_no_ws
                                                in
                                                    (SOME (Scalar (scalar_content, SingleQuoted)), next_state)
                                                end
                                          | SOME #"\"" =>
                                                let
                                                    val (scalar_content, next_state) = parse_double_quoted_scalar state_no_ws
                                                in
                                                    (SOME (Scalar (scalar_content, DoubleQuoted)), next_state)
                                                end
                                          | SOME #">" =>
                                                let
                                                    val next_state = advance state_no_ws
                                                    fun collect_folded (acc, s) =
                                                        case peek s of
                                                            SOME #"\n" =>
                                                                let
                                                                    val s' = advance s
                                                                in
                                                                    case peek s' of
                                                                        SOME c =>
                                                                            if c = #" " orelse c = #"\t"
                                                                            then
                                                                                let
                                                                                    val (line, s'') = consume_while (fn x => x <> #"\n") s'
                                                                                in
                                                                                    collect_folded (acc ^ "\n" ^ line, s'')
                                                                                end
                                                                            else (acc, s')
                                                                        | NONE => (acc, s')
                                                                end
                                                          | SOME c =>
                                                                let
                                                                    val (line, s') = consume_while (fn x => x <> #"\n") s
                                                                in
                                                                    collect_folded (acc ^ line, s')
                                                                end
                                                          | NONE => (acc, s)
                                                    val (scalar_content, final_state) = collect_folded ("", next_state)
                                                in
                                                    (SOME (Scalar (scalar_content, Fold)), final_state)
                                                end
                                          | SOME #"#" =>
                                                let
                                                    (* Skip comment until end of line and get next token *)
                                                    val (_, next_state) = consume_while (fn c => c <> #"\n") state_no_ws
                                                in
                                                    (* Continue to next token instead of returning comment *)
                                                    next_token next_state
                                                end
                                          | SOME c =>
                                                if Util.isAlphaNum c orelse c = #"_" then
                                                    let
                                                        val (scalar_content, next_state) = parse_plain_scalar state_no_ws
                                                    in
                                                        (SOME (Scalar (scalar_content, Plain)), next_state)
                                                    end
                                                else
                                                    (* Skip unknown character and try again *)
                                                    next_token (advance state_no_ws)
                                end
                    end

    (* Simple tokenize function that collects all tokens into a list *)
    fun tokenize (input: string) : token list =
        let
            val initial_state = initialState input
            
            fun collect_tokens (state: tokenizer_state, acc: token list) : token list =
                case next_token state of
                    (NONE, _) => rev (StreamEnd :: acc)
                  | (SOME token, new_state) => collect_tokens (new_state, token :: acc)
        in
            collect_tokens (initial_state, [StreamStart])
        end

    (* Create a tokenizer that can be used iteratively by the parser *)
    fun make_tokenizer (input: string) : tokenizer_state =
        initialState input

    (* For debugging: convert token to string *)
    fun token_to_string (token: token) : string =
        case token of
            StreamStart => "StreamStart"
          | StreamEnd => "StreamEnd"
          | DocumentStart => "DocumentStart"
          | DocumentEnd => "DocumentEnd"
          | BlockSequenceStart => "BlockSequenceStart"
          | BlockMappingStart => "BlockMappingStart"
          | BlockEnd => "BlockEnd"
          | FlowSequenceStart => "FlowSequenceStart"
          | FlowSequenceEnd => "FlowSequenceEnd"
          | FlowMappingStart => "FlowMappingStart"
          | FlowMappingEnd => "FlowMappingEnd"
          | Key => "Key"
          | Value => "Value"
          | BlockEntry => "BlockEntry"
          | FlowEntry => "FlowEntry"
          | Scalar (content, style) => 
                let
                    val style_str = case style of
                        Plain => "Plain"
                      | SingleQuoted => "SingleQuoted"
                      | DoubleQuoted => "DoubleQuoted"
                      | Literal => "Literal"
                      | Fold => "Fold"
                in
                    "Scalar(\"" ^ content ^ "\", " ^ style_str ^ ")"
                end
          | Comment content => "Comment(\"" ^ content ^ "\")"
          | Tag content => "Tag(\"" ^ content ^ "\")"
          | Indent => "Indent"
          | Dedent => "Dedent"
end