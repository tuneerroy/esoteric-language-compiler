import Test.HUnit ()
import Test.QuickCheck ()
import WParserTest qualified

main :: IO ()
main = do
  putStrLn "Running Parser Tests"
  WParserTest.qc
