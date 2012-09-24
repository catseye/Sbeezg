The Sbeezg Programming Language
===============================

v2002.0317

Sbeezg is single-assignment programming taken to the extreme.  Each variable
may only be assigned once.  There is no scope.  When a function is executed,
a new copy of it is made, with all bound variable names altered to fresh ones,
and this is executed instead.  Execution is sequential in nature.  Arguments
and return value are given explicitly.  There are no global names; there are
only lambda function definitions available.  There are five built-in operations.
For convenience, there are three data types: atoms, integers, and closures.

Here is a brief EBNF rundown of the syntax:

    Appl ::= Name "=" Val "(" Val  {"," Val} ")".
    Val  ::= Name | "*" Const | "{" Name {"," Name} "|" Appl {";" Appl} "|" Name "}".

A program is an application, which consists of an assignment to a new (never
before named in the program) variable, of a value or the result of a function
call.  Note that the arguments of a function call may only be simple values;
further nested function calls are disallowed as their implicit 'piping' of
values from one function to the next without an intervening variable name is
counter to the intention of this purely single-assignment language.

This documentation isn't really complete.

Chris Pressey  
March 17 2002  
Winnipeg, Manitoba
