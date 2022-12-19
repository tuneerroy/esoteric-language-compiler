import UnitTests qualified
import WInterpreterTest qualified
import WParserTest qualified

main :: IO ()
main = do
  -- putStrLn "s"
  -- return ()
  putStrLn "Running Parser Tests"
  WParserTest.qc
  WInterpreterTest.qc
  WParserTest.qc
  -- UnitTests.main
  UnitTests.testWSAsterikGrid

-- CompilerTest.qc