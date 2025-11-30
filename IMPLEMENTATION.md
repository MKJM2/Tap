In the top-level grammar.ebnf file, you will find a EBNF description of a grammar for the
Tap programming language.

There was a previous version of 'Tap', with a corresponding previous
implementation. Since then, the grammar for `Tap` has been redesigned.

Handle Unicode correctly (use .chars()) as we will have to handle Polish langauge syntax eventually.
Tokens should have spans (start/end char offsets) for better error reporting.

As for the parser: it will be a recursive-descent, context-aware parser. Every nonterminal becomes a method.
Hard context-sensitive rules (e.g., forbidding assignment to non-lvalues, checking pattern validity, enforcing keyword vs identifier distinctions) must be enforced during parsing.
All productions must be written in a style that is readable, correct, and testable.
Produce a well-typed AST with enums and structs. Errors should be helpful and provide context
(what production were we trying to parse?)

As for the interpreter: A tree-walking interpreter

Lexically scoped environments.

First-class functions + closures (lexical capture). Strong runtime error diagnostics with spans.

Start by generating tests for the language. Do good Test Driver Development. Any ambiguities in grammar should be resolved
by referencing the grammar.ebnf file. It is the single source of truth on the grammar. For a basic source of tests look into the
top-level TESTS.md file. You WILL HAVE to update this file as you implement more tests.

Your MAIN, PRIMARY task right now: write the tests as per TESTS.md. Don't worry about whether they pass or not.
The currently written tests are wrong. You will have to fix them as you go along. But first, write the tests.
Remember that for grammar reference you can refer to grammar.ebnf.
For syntax reference, refer to the README.md which provides quite a few useful syntax example constructs.
