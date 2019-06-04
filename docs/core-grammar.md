# Semantic Core grammar

This is an EBNF grammar for the (experimental) core IR language.

```
expr ::= expr '.' expr
       | expr ' '+ expr
       | '{' expr (';' expr)* ';'? '}'
       | 'if' expr 'then' expr 'else' expr
       | ('lexical' | 'import' | 'load') expr
       | lit
       | 'let'? lvalue '=' expr
       | '(' expr ')'

lvalue ::= ident
         | parens expr

lit ::= '#true'
      | '#false'
      | 'unit'
      | 'frame'
      | lambda
      | ident

lambda ::= ('λ' | '\') ident ('->' | '→') expr

ident ::= [A-z_] ([A-z0-9_])*
        | '#{' [^{}]+ '}'
        | '"' [^"]+ '"'
```
