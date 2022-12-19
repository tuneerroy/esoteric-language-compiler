import CompilerTest qualified
import UnitTests qualified
import WInterpreterTest qualified
import WParserTest qualified

main :: IO ()
main = do
  -- putStrLn "s"
  -- return ()
  putStrLn "Running Parser Tests"

  -- UnitTests.testBFCat

  -- WParserTest.qc
  -- WInterpreterTest.qc
  -- WParserTest.qc
  -- CompilerTest.qc

  -- UnitTests.run100_2 -- WORKS

  UnitTests.testWSAsterikGrid -- NO

-- UnitTests.testWSFib

-- UnitTests.testWSHelloWorld -- WORKS
-- UnitTests.testWSDivDoub

-- UnitTests.main
--UnitTests.testWSAsterikGrid