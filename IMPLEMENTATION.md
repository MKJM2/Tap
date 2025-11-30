In the top-level grammar.ebnf file, you will find a EBNF description of a grammar for the
Tap programming language.

Handle Unicode correctly (use .chars()) as we will have to handle Polish langauge syntax eventually.
Tokens should have spans (start/end char offsets) for better error reporting.

As for the parser: it will be a recursive-descent, context-aware parser. Every nonterminal becomes a method. Each method should have a helpful doxy comment,
including a snippet of the relevant EBNF production.
Hard context-sensitive rules (e.g., forbidding assignment to non-lvalues, checking pattern validity, enforcing keyword vs identifier distinctions) must be enforced during parsing.
All productions must be written in a style that is readable, correct, and testable.
Produce a well-typed AST with enums and structs. Errors should be helpful and provide context
(what production were we trying to parse?). Each

As for the interpreter: A tree-walking interpreter.
Lexically scoped environments. Braces are closures.

First-class functions + closures (lexical capture). Strong runtime error diagnostics with spans.

Start by generating tests for the language. Do good Test Driver Development. Any ambiguities in grammar should be resolved
by referencing the grammar.ebnf file. It is the single source of truth on the grammar. For a basic source of tests look into the
top-level TESTS.md file. You WILL HAVE to update this file, checking off implemented tests as you implement more tests.

Your MAIN, PRIMARY task right now: write the tests as per TESTS.md. Don't worry about whether they pass or not.
Some of the currently written tests may be wrong. You will have to fix them as you go along. But first, write the tests and make sure they compile,
and not necessarily that they pass.
Remember that for grammar reference you can refer to grammar.ebnf, which is the SINGLE SOURCE OF TRUTH on the grammar.
For syntax reference, refer to the README.md which provides quite a few useful syntax example constructs.
NO mocking please. Write real tests that run the real lexer, parser, & interpreter.

For now, implement more parser tests. Once you have a good number of parser tests, get back to me for further instructions.

Periodically refer back to this file to recall your top-level goals.
