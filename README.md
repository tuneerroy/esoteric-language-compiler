# Esoteric Language Compiler

Final project for 2022 Fall CIS 5520.

Group Members:  
Tuneer Roy _(github: tuneerroy, pennkey: tuneer)_  
Nicholas Liu _(github: liunicholas6, pennkey: liun0)_  
Richard Chai _(github: richardchai, pennkey: richchai)_

_Note:_ Much work was done using Visual Studio's Live Share extension. **Most of the work was also done in a single all-nighter so ignore the commit messages and random files in the repo. :)**

## File overview:

## Src:

### Parser files: Parser.hs, BParser.hs, WParser.hs

Parser.hs describes generic parser functions, while BParser.hs and WParser.hs use those functions to parse brainfuck and whitespace respectively into our internal data type representation of each language.

### Syntax files: ASyntax.hs, BSyntax.hs, WSyntax.hs, NonNeg.hs

ASyntax.hs describes our generic assembly syntax (which is not assembly language specific!) If we had more time we could extend this to more assembly languages. BSyntax.hs and WSyntax.hs describe our data types that represent Brainfuck and Whitespace respectively.

NonNeg.hs describes nonnegative integers which serve constrain certain arguments in WSynatax.

### Compiler files: BCompiler.hs, WCompiler.hs, Compiler.hs

BCompiler.hs and WCompiler.hs describe compilers for Brainfuck and Whitespace with language-specific translations of our abstract language syntax to our abstract assembly syntax. Compiler.hs was our first attempt at a compiler to be shown at the demo to show how our program has progressed.

### Interpreter files: BInterpreter.hs, WInterpreter.hs, JumpProgram.hs, MonadReadWrite.hs

B/WCompiler.hs describe interpreters for Brainfuck and Whitespace respectively. They are mainly for use in our testing, as they allow us to run programs in their native language (using our interpreters) and compare that to the output from running the compiled versions using our computer's native architecture.a

JumpProgram.hs takes a list of commands with labeled jumps and turns it to an array with index targets. This way, jumps can be efficiently executed.

MonadReadWrite.hs describes a typeclass for handling input and output, so that pure substitutes for IO can be used in place of it for testing. It also has lifts through monad transformers.

## Test files:

### Testing files: WArbPrograms.hs

WarbPrograms.hs provides generators for programs of differing levels of complexity. At the base level there are programs that only modify the stack, then there are programs that also do IO, and then there are programs that perform heap operations.

By keeping track of the number of pushes and pops that each stack instruction performs, we can generate arbitrary jump-free programs that don't pop an empty stack - the most common error in Whitespace.
