# Whitespace Compiler

Final project for 2022 Fall CIS 5520.

By Tuneer Roy (tuneerroy), Nicholas Liu (liunicholas6), and Richard Chai (richardchai)

PennKeys:
Tunner Roy (tuneer)
Nicholas Liu (liun0)
Richard Chai (richchai)

## File overview:

## Src:

### Parser files: Parser.hs, BParser.hs, WParser.hs

Parser.hs describes generic parser functions, while BParser.hs and WParser.hs use those functions to parse brainfuck and whitespace respectively into our internal data type representation of each language.

### Syntax files: ASyntax.hs, BSyntax.hs, WSyntax.hs

ASyntax.hs describes our generic assembly syntax (which is not assembly language specific!) If we had more time we could extend this to more assembly languages. BSyntax.hs and WSyntax.hs describe our data types that represent Brainfuck and Whitespace respectively.

### Compiler files: BCompiler.hs, WCompiler.hs, Compiler.hs

BCompiler.hs and WCompiler.hs describe compilers for Brainfuck and Whitespace with language-specific translations of our abstract language syntax to our abstract assembly syntax. Compiler.hs was our first attempt at a compiler to be shown at the demo to show how our program has progressed.

### Interpreter files: BInterpreter.hs, WInterpreter.hs, MonadReadWrite.hs

B/WCompiler.hs describe interpreters for Brainfuck and Whitespace respectively. They are mainly for use in our testing, as they allow us to run programs in their native language (using our interpreters) and compare that to the output from running the compiled versions using our computer's native architecture.a

MonadReadWrite.hs uses monad transformers to

### Testing files: JumpProgram.hs,
