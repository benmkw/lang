# lang

![](https://github.com/benmkw/my_lang/workflows/CI/badge.svg)

This is very much wip progress of me learning rust and language implementation.

Eventually this may turn into a parser and some backend that can deal with a c like language which may be a subset of rust.

The backend may be a bytecode VM and/ or cranelift/ qbe/ llvm.

Currently it is a tree walking interpreter for arithmetic which honors left and right associativity as well as precedence.
It seems to be very slow which should be investigated at some point but it not a priority right now.

It is build after [craftinginterpreters](https://github.com/munificent/craftinginterpreters) by Bob Nystrom whom I want to thank for his very good free book.

I used a very early [commit](https://github.com/munificent/craftinginterpreters/blob/17b744787a296e9dd57ac7b1af87486da4ca7f2f/c/compiler.c) to reduce the complexity and searched for other simple implementations but because my understanding was still lacking I tried to reduce it even further and implement it myself.

My motivation for just implementing arithmetic operations was to really understand pratt parsing with as little overhead (read: no features of the "language" but just understand the flow of the parsing procedures themselves) as possible and only do the key concepts first.

I implemented a scheme interpreter after [Peter Norvig](https://norvig.com/lispy.html) next to understand more complex evaluation without the overhead of worrying much about parsing. The next step is to combine both parts of knowledge.
