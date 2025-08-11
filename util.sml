structure Util =
struct
    fun trim s = 
        let
            fun trimLeft [] = []
              | trimLeft (c::cs) = if Char.isSpace c then trimLeft cs else c::cs
            fun trimRight s = String.implode (List.rev (trimLeft (List.rev (String.explode s))))
        in
            trimRight (String.implode (trimLeft (String.explode s)))
        end


    fun split_lines s = String.tokens (fn c => c = #"\n") s

    fun isSpace c = (c = #"\t" orelse c = #" " orelse c = #"\n" orelse c = #"\r")

    (* Checks if a character is a digit (0-9). *)
    fun isDigit c = (Char.isDigit c)

    (* Checks if a character is alphabetic (letter). *)
    fun isAlpha c = (Char.isAlpha c)

    (* Checks if a character is alphanumeric (letter or digit). *)
    fun isAlphaNum c = (isAlpha c orelse isDigit c)

end