# Groups

This post is about [groups](https://en.wikipedia.org/wiki/Group_(mathematics)) (whouldathunkit).

## Conventions/Notation

`forall var: prop` means `prop` is true for all possible `var`.
`thereis var: prop` means `prop` is true for at least one `var`.
`thereis1 var: prop` means `prop` is true for exactly one `var`.
`forall var1: forall var2: prop` will be abbreviated as `forall var1, var2: prop`.

Sets are written as its elements surrounded by brackets. For example,
`{}` is the empty set and `{ a, b, c }` is a set with the elements
`a`, `b` and `c`. Sets comprehension will be written as `{ expr : vars, restrictions }`
and the cardinal of a set will be written as `#S`.

When you see _N0_, _Z2_, _Zn_, [m]n, etc, pretend the character to the right is in subscript.

_N_ is the set of natural numbers (no zero).
_N0_ is the set of natural numbers (and zero).
_Z_ is the set of integers.

`%` will be used as the modulo operator. Some examples: `4 % 5 = 9 % 5 = 4`, `5 % 2 = 1`.
This operator is called `%` in C, `modulo` in Scheme and `mod` in Haskell.
Another useful definition, is: `p = q (mod n) <=> p % n = q % n`
`|` will be used to mean "divides", as in `a | b` means "a divides b",
"b is a multiple of a", or, with modulo, `a | b <=> b % a = 0`

We will be needing the concept of a "Congruential Equivalence Class" later on.
They are written as `[m]n`, forall n in N, m in Z, and are defined as
`[m]n = { x : x in Z, x = m (mod n) }`.

Depending on context, `+` and `*` will either be the usual addition and
multiplication of numbers, or addition and multiplication of classes.
Addition and multiplication of classes are defined as `[p]n + [q]n = [p+q]n`
and `[p]n * [q]n = [p*q]n`.
Some examples: `2+2=4`, `[2]3 + [1]3 = [2+1]3 = [0]3`

## What is a Group?!

### In general

A group is just a pair (S, O), where S is a set and O is a binary operation
on S (`O : S*S -> S`) with the following properties:

 1. _Associativity_: forall a, b, c in S: O(a, O(b, c)) = O(O(a, b), c)
 2. _Identity_: thereis1 id in S: forall a in S: O(a, id) = O(id, a) = a
 3. _Inverse_: forall a in S: thereis1 a' in S: O(a, a') = O(a', a) = id
     id always has inverse, itself

There is one extra property:

 1. _Commutativity_: forall a, b in S: O(a, b) = O(b, a)

A group with a commutative operation is called a commutative group or an abelian group.

Given G = (G, O) a group, G will be used to refer both to the group
itself and its associated set.

A group G is said to be infinite if #G is infinite, and finite if #G is finite.

That is the generic definition. We will focus on groups with integer sets
and `+` or `*` as the operation, however, so to make things easier:

### Integers

When the operation is + we call the identity element "null element" and represent it with 0.
When the operation is * we call the identity element "unity element" and represent it with 1.

When the operation is + we call the inverse of an element a its symmetric and represent it as -a.
When the operation is * we call the inverse of an element a its inverse and represent it as a^-1. (pretend -1 is in superscript)

#### (N0, +)

N0 is the set of natural numbers (with zero), and `+` is the usual addition on natural numbers.
Is it a group?

 1. forall a, b, c in N0: a + (b + c) = (a + b) + c
 2. 0 is the identity of the group, because forall a in N0: a + 0 = 0 + a = a
 3. forall a, b in N: a + b != 0

N0 does not satisfy R3, so it is not a group.

#### (Z, +)

Z is the set of integers, and `+` is the usual addition on integers.
Is it a group?

 1. forall a, b, c in Z: a + (b + c) = (a + b) + c
 2. 0 is the identity of the group, because forall a in Z: a + 0 = 0 + a = a
 3. forall a in Z: thereis1 a' in Z: a + a' = 0

Z satisfies all 3 requirements so it is a group. We also know that addition
is commutative, so Z is an abelian group.

#### Other groups and non-groups

 * (R, +), where R is the set of real numbers, is a group
 * (R\\{0}, *) is also a group
 * (R, *) is not a group, because 0 has no inverse (R3)
 * (N, +) is not a group, because there is no identity and no inverse (R2 and R3)
 * (N, *) is not a group, because there is no inverse (R3)
 * (Z\\{0}, *) is not a group, because other than 1 and -1, no element has inverse (R3)

## What's next?

Some useful definitions follow.

### Subgroup

Given a group G, H is called a subgroup of G if H is contained in G and
it is also a group, and we write `H <| G`. Examples:

 * `forall (G, +) group: (G, +) <| (G, +)`
 * _Trivial Subgroup_: `forall (G, +) group: ({0}, +) <| (G, +)`
 * `(2Z, +) <| (Z, +)`

A subgroup H of a group G that is not G itself (H != G, or H is strictly contained in G)
is called a "Proper Subgroup", and we write `H < G`. Examples:

 * _Trivial Subgroup_: `forall (G, +) group: ({0}, +) < (G, +)`
 * `(2Z, +) < (Z, +)`

### Multiples/Powers of an element

Given an element a of an additive group, `a + ... + a` (n times) can be
written as `n * a`. Special case: `0 * a = 0`.
Given an element a of a multiplicative group, `a * ... * a` (n times)
can be written as `a ^ n`. Special case: `a ^ 0 = 1`.

For negative n, `n * a = -((-n) * a) = (-n) * (-a)`.
For negative n, `a ^ n = (a ^ (-n)) ^ -1 = (a ^ -1) ^ (-n)`.

### Order of an element

The order of an element a is the minimum number of times it must be operated
with itself until it reaches the identity, and is written as `o(a)`. If,
no matter how many times you operate the element, it doesn't reach the
identity, its order is said to be infinite.

A more rigorous definition is:

 * a has _infinite order_ if forall n in N: n * a != 0
 * a has _finite order_ k, i.e., o(a) = k if:
     1. k is in N
     2. k * a = 0
     3. forall n in N: `n * a = 0` => `n <= k`

In particular, the order of the identity is 1, and the identity is the only element with order 1.
Useful fact: forall G group: forall a in G: o(a) | #G. From this comes the fact that,
in a finite group, no element has infinite order.

### Generated Subgroup

Given an additive group (G, +) and an element a, `<a> = { n * a : n in Z }` is a subgroup of G
and is called the "Subgroup of G generated by a". In particular, if `G = <a>`, a is said to generate G,
or that G is generated by a. Note that `#<a> = o(a)`.
An example of this is `<1> = <-1> = Z`.

## Zn Groups

You can group integers together according to their (mod n), make a set
out of them, and define a group with it. These groups are called _Zn_,
for some n in N, and are defined as Zn = { [m]n : m in { 0, ..., n-1 } }.

These will get repetitive after Z2, but the reason why they're here will
become clear.

### (Z1, +)

According to definition above, Z1 = { [0]1 }, but you can go further here:

[0]1
 = (def [m]n)
{ x : x in Z, x = 0 (mod 1) }
 = (def (mod n))
{ x : x in Z, x % 1 = 0 % 1 }
 = (0 % 1 = 0)
{ x : x in Z, x % 1 = 0 }
 = (every integer is a multiple of 1)
{ x : x in Z }
 = (def Z)
Z

Z1 is a trivial group (#Z1 = 1), so it isn't that interesting, other
than Z being its only element.

Some things we can find about it:

 * Since it is a trivial group, its only subgroup is itself
 * [0]1 is the identity, so o([0]1) = 1
 * Z1 = <[0]1>

### (Z2, +)

When n = 2 we get Z2 = { [0]2, [1]2 }.

[0]2
 = (def [m]n)
{ x : x in Z, x = 0 (mod 2) }
 = (def (mod n))
{ x : x in Z, x % 2 = 0 % 2 }
 = (0 % 2 = 0)
{ x : x in Z, x % 2 = 0 }
 = (x % 2 = 0 => thereis k in Z: x = 2 * k)
{ 2 * x : x in Z }
 = (def 2Z)
2Z

So [0]2 = 2Z is the set of even integers. You can probably guess by now, but:

[1]2
 = (def [m]n)
{ x : x in Z, x = 1 (mod 2) }
 = (def (mod n))
{ x : x in Z, x % 2 = 1 % 2 }
 = (1 % 2 = 1)
{ x : x in Z, x % 2 = 1 }
 = (x % 2 = 1 => thereis k in Z: x = 2 * k + 1)
{ 2 * x + 1 : x in Z }
 = (def 2Z+1)
2Z+1

And with that you see that [1]2 is the set of odd integers.

Some things we can find about it:

 * [0]2 is the identity, so o([0]2) = 1
 * [1]2 != [0]2, but [1]2 + [1]2 = [1 + 1]2 = [2]2 = [0]2, so o([1]2) = 2
 * <[0]2> = { [0]2 } < Z2
 * <[1]2> = Z2

### (Z3, +)

Z3 = { [0]3, [1]3, [2]3 }

I'll skip showing how to get to the definition of each of the classes
from now. Also, you may have already noticed, but if not, [0]n is the
set of multiples of n, nZ.

[0]3 = 3Z
[1]3 = 3Z + 1
[2]3 = 3Z + 2

And now things about Z3:

 * o([0]3) = 1
 * o([1]3) = 3; you may also have noticed that o([1]n) = n. This has
     to do with the fact that forall n in Z: n * 1 = n.
     So n * [1]n = [n * 1]n = [n]n = [0]n
 * o([2]3) = 3; [2]3 != [0]3, 2 * [2]3 = [1]3 != [0]3, 3 * [2]3 = [0]3
     This one has to do with the fact that 2 and 3 are coprime, which
     means lcm(2, 3) = 6, and 6 / 2 = 3
 * <[0]3> = { [0]3 } < Z3
 * <[1]3> = <[2]3> = Z3

### (Z4, +)

Z4 = { [0]4, [1]4, [2]4, [3]4 }

[0]3 = 3Z
[1]3 = 3Z + 1
[2]3 = 3Z + 2

Once again, things about this group:

 * o([0]4) = 1
 * o([2]4) = 2
 * o([1]4) = o([3]4) = 4
 * <[2]4> = { [0]4, [2]4 } < Z4
 * <[1]4> = <[3]4> = Z4

Now something interesting happened here! Remember that
  forall G group: forall a in G: o(a) | #G
Because of this we know that the only possible orders are 1, 2 and 4.
This also means that it is possible a subgroup of cardinal 2 exists.
In this case, the only one is <[2]4>. Why, though, did this happen with
Z4, but not with Z1, Z2 or Z3? Z1 is obvious: #Z1 = 1, so the only possible
subgroup is itself. Z2 is also easy: its only elements are [0]2 and [1]2,
and we know that
  forall n in N: <[0]n> = { [0]n }
and that
  forall n in N: <[1]n> = Zn
For Z3 it's not as clear. The hint is, again:
  forall G group: forall a in G: o(a) | #G
Both 2 and 3 are prime, which means, their only divisors are 1 and themselves.
So it's not possible for a subgroup of Z3 with cardinal 2 to exist.

### (Z5, +)

Try this one

Summing up:
  forall n in N: Zn has more than one proper subgroup <=> n is not prime
or
  forall n in N: Zn has exactly one proper subgroup <=> n is prime

Proving this is easy: Let n be in N.

Suppose that Zn has more than one proper subgroup. We want to prove that
n is not prime. There is a proper subgroup H such that **#H > 1**. Since H
is a proper subgroup, then **#H < n**. From this, and the fact that #H | n,
we can conclude n is not prime.

Now the other way: Suppose n is not prime. We want to prove that Zn has
more than one proper subgroup. Since n is not prime, then
  thereis k in N: `k < n` and `k > 1` and k | n
Let p such a number. Then
  thereis a in Zn: o(a) = k
Let a be such an element. **o(a) = k => #\<a> = k**
We know that **\<a> <| Zn** and that **k < n**, so **\<a> < Zn**.

**TODO:** groups that are isomorphic to some proper subgroup
