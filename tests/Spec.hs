import Arm64ToStringTest (tArm64ToString)
import BParserTest qualified
import CompilerTest qualified
import Test.HUnit (runTestTT)
import UnitTests qualified
import WInterpreterTest qualified
import WParserTest qualified

main :: IO ()
main = do
  runTestTT tArm64ToString
  WParserTest.qc
  BParserTest.qc
  WInterpreterTest.qc
  CompilerTest.qc
  UnitTests.main