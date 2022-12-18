module WCompiler where

import ASyntax (AInstruction (..), Reg64 (..))
import WParser (Token (..), WLabel (..))
import WSyntax (WBop (..), WCond (Any, Neg, Zero), WInstruction (..))

compileCommand :: (Eq a, NonWhitespaceShow a) => WInstruction a -> [AInstruction]
compileCommand i = case i of
  InputChar -> []
  InputNum -> []
  OutputChar ->
    [ Comment "outputChar",
      MovI (Reg 0) 1,
      GetAddress (Reg 1) "buf",
      Ldr (Reg 8) SP 0,
      Str (Reg 8) (Reg 1),
      MovI (Reg 2) 1,
      MovI (Reg 16) 4,
      Svc
    ]
  OutputNum ->
    []
  Push n ->
    []
  Dup ->
    []
  Swap ->
    []
  Discard ->
    []
  Copy n ->
    []
  Slide n ->
    []
  Arith Add ->
    []
  Arith Sub ->
    []
  Arith Mul ->
    []
  Arith Div ->
    []
  Arith Mod ->
    []
  Label a ->
    []
  Call a ->
    []
  Branch Any a ->
    []
  Branch Zero a ->
    []
  Branch Neg a ->
    []
  Return ->
    []
  End ->
    []
  Store ->
    []
  Retrieve ->
    []

class NonWhitespaceShow a where
  toString :: a -> String

instance NonWhitespaceShow WLabel where
  toString (WLabel t) = foldr (\x acc -> tokenToChar x : acc) "" t

tokenToChar :: Token -> Char
tokenToChar Space = 'S'
tokenToChar Tab = 'T'
tokenToChar LF = 'L'
