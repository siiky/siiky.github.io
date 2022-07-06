% Small Big Numbers
% siiky
% 2019/10/01

Code can be found at [siiky/c-utils](https://github.com/siiky/c-utils).

The idea is to make a really simple and small (not going for performance)
big numbers library. There are good bignum libs out there, most likely
with much better performance than this one, but none is small enough to
embed in a single file. The goal is to use it in programming competitions,
where a single source file may be submitted, often with file size limits.

Requirements:

 - [ ] Reading/writing from/to string
     - [ ] Base 10
     - [ ] Base 16
 - [ ] Convert from machine integers (64, 32, ...)
 - [ ] Adding
 - [ ] Subtracting
 - [ ] Multiplying
 - [ ] Dividing
