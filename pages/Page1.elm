module Page1 exposing (output)

-- First block of code


output =
    List.sum (List.range 0 3)



-- Second block of code
-- which continues on the
-- next few rows!


a =
    11



-- A block with some _very_ important **MARKDOWN**
-- which lets us
-- # Establish
-- ## A hierarcy
-- of text


b =
    12


c =
    a + b


multiline =
    c + a


strings =
    String.concat (List.repeat 4 "abcdefg")
