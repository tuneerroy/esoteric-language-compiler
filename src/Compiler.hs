module Compiler where

import ASyntax -- (AInstruction)
-- (WInstruction)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import WParser
import WSyntax

compileCommand :: (Eq a, Stringable a) => WInstruction a -> [String]
compileCommand i = case i of
  -- TODO: handle IO
  InputChar -> [" ; input character"]
  InputNum -> [" ; input number"]
  OutputChar -> [" ; output character"]
  OutputNum -> [" ; output number"]
  Push n ->
    [ "; push",
      "PUSH {" ++ show n ++ "}"
    ]
  Dup ->
    [ "; dup",
      "LDR r1, [sp]",
      "SUB sp, sp, #4",
      "STR r1, [sp]"
    ]
  Swap ->
    [ "; swap",
      "POP {r1, r2}",
      "PUSH {r2, r1}"
    ]
  Discard ->
    [ "; discard",
      "ADD sp, sp, #4"
    ]
  Copy n ->
    -- TODO: is this the top n values or the bottom n values of the stack? the latter seems a bit pain :'(
    [ "; copy"
    ]
  Slide n ->
    [ "; slide",
      "ADD sp, sp, #" ++ show (n * 4)
    ]
  Arith wb ->
    [ "; arith",
      "POP {r1, r2}",
      compileArith wb ++ " r3, r1, r2", -- TODO: handle division & mod
      "PUSH {r3}"
    ]
  Label a ->
    [ "; label",
      toString a
    ]
  Call a ->
    [ "; call",
      ""
    ]
  Branch wc a ->
    [ "; branch",
      ""
    ]
  Return ->
    [ "; return",
      ""
    ]
  End ->
    [ "; end",
      ""
    ]
  Store ->
    [ "; store",
      ""
    ]
  Retrieve ->
    [ "; retrieve",
      ""
    ]

compileProgram :: (Eq a, Stringable a) => [WInstruction a] -> [String]
compileProgram = concatMap compileCommand

compileArith :: WBop -> String
compileArith Add = "ADD"
compileArith Sub = "SUB"
compileArith Mul = "MUL"
compileArith Div = undefined
compileArith Mod = undefined

test :: String -> Maybe [String]
test s = do
  commands <- wParseString s
  return (compileProgram commands)

class Stringable a where
  toString :: a -> String

instance Stringable WLabel where
  toString (WLabel t) = foldr (\x acc -> tokenToChar x : acc) "" t

tokenToChar :: Token -> Char
tokenToChar Space = 'S'
tokenToChar Tab = 'T'
tokenToChar LF = 'L'

-- >>> toString (WLabel (Space :| [Tab, LF, Space]))
-- "STLS"
