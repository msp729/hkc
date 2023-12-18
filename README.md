# HKC
a nice little calculator for my own personal usage.

# Features
## Polish Notation
`+ 1 2` is `3`, and `exp - ln 21 ln 7` is also three (approximately).
## Variables!
Prefix `:a`, `:b`, & `:c` to store (up to) three values!
Also, `ans` does the same thing as on every other calculator.
## Functions!
Prefix `:f` and use `x` to define a unary (one-argument) function,
`:g` with `x` and `y` for a binary, and `:h` with `x`, `y`, and `z` for a ternary!

NB: functions do not close over their environments, e.g.
```
%> :a 1
1.0
%> :f / x a
%> f 3
3.0
%> :a f 2
2.0
%> f 4
2.0
```
