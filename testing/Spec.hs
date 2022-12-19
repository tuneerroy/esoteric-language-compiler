import CompilerTest qualified
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
  CompilerTest.qc

-- UnitTests.testWS1To100
-- UnitTests.testWSCat
-- UnitTests.testWSAsterikGrid
-- UnitTests.testWSDivDoub

-- UnitTests.main
--UnitTests.testWSAsterikGrid