import CompilerTest qualified
import UnitTests qualified
import WInterpreterTest qualified
import WParserTest qualified

main :: IO ()
main =
  do
    -- putStrLn "s"
    -- return ()
    -- putStrLn "Running Parser Tests"
    -- UnitTests.testWS99Bottles
    -- UnitTests.testWSFib

    -- UnitTests.testBFCat

    -- WParserTest.qc
    -- WInterpreterTest.qc
    -- WParserTest.qc
    CompilerTest.qc

-- UnitTests.testWSHelloWorld -- WORKS
-- UnitTests.run100_2 -- WORKS

-- NO

-- UnitTests.testWSDivDoub

-- UnitTests.main

-- UnitTests.testWSTruthMachine