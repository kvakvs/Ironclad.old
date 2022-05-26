# Parser: Operators and Precedence

## Precedence

> Source: [Operator Precedence](https://www.erlang.org/doc/reference_manual/expressions.html#operator-precedence)

| Operator                    | Notes             |
|-----------------------------|-------------------|
| :                           |                   |
| #                           |                   |
| Unary + - bnot not          |                   |
| / * div rem band and        | Left associative  |
| + - bor bxor bsl bsr or xor | Left associative  |
| ++ --                       | Right associative |
| == /= =< < >= > =:= =/=     |                   |
| andalso                     |                   |
| orelse                      |                   |
| = !                         | Right associative |
| catch                       |                   |

## Term Comparisons

> Source: [Term Comparisons](https://www.erlang.org/doc/reference_manual/expressions.html#term-comparisons)
