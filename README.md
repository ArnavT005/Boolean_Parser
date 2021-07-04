# Boolean Parser | ML-Lex, ML-Yacc
## Assignment of course COL226, Spring 2021

---
### About: 

This project involves building a toy programming language that is capable of evaluating and type-checking a set of statements which may be boolean/arithmetic expressions, variable instantiations and function declaration/application upto a single formal parameter. The language also supports lambda expressions and uses **call-by value** method for function application.

---

### Language specifications and Syntax:

Refer to *Specifications* folder to know more about the language syntax and usage. This folder includes Problem_Statement.pdf, EBNF.txt describing the language syntax and examples demonstrating the usage of various expressions.

---

### Instructions to execute a program:
- Generate the parser/evaluator/type-checker by executing the command `make` or `make all` in the terminal.
- To run a program file named <file>, execute the command `./parser <file>` in the terminal. This will parse, evaluate and type-check the file named `<file>`.
> NOTE: `mlton` (along with `mllex` and `mlyacc`) should be available for proper execution of makefile.   
> NOTE: The parser creates three auxilliary files named *Yes*, *Error* and *lastToken*. These files are used for reporting syntax error raised by the parser.

- After execution, use `make clean` to clear all the extra files (like .sig, lex.sml, aux files etc.)

---
---
