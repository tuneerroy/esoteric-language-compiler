import BParserTest qualified
import CompilerTest qualified
import Test.HUnit ()
import Test.QuickCheck ()
import UnitTests qualified
import WInterpreterTest qualified
import WParserTest qualified

main :: IO ()
main = do
  putStrLn "Running Parser Tests"
  WParserTest.qc
  WInterpreterTest.qc
  BParserTest.qc
  UnitTests.test
  CompilerTest.qc