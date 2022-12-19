import CompilerTest qualified
import Test.HUnit ()
import Test.QuickCheck ()
import WInterpreterTest qualified
import WParserTest qualified

main :: IO ()
main = do
  putStrLn "Running Parser Tests"
  WParserTest.qc
  WInterpreterTest.qc
  CompilerTest.qc