% Faucets
% siiky
% 2020/02/20~2021/07/23

Recently I asked a bunch of people which one out of two types of faucets they
prefer. The answers were pretty surprising (to me), as everyone chose the same
type. In this post I'll prove them wrong.

# The Problem

The state of a faucet determines 2 things: (1) water output pressure; and (2)
water temperature. In real life, many things, including external factors,
affect water temperature: water pressure (especially for hot watter), if the
pipes are exposed to the sun, rain, etc. Also in real life, it's not uncommon
for the maximum hot water pressure to be lower than that of cold water.
However, to [KISS], we won't take these things into account. In the case of
pressure, we'll just assume it's the same for both hot and cold water.

So, in our simplified model, the state of a faucet alone will determine the
water state: both water temperature and output pressure.

The plan is to prove that the faucet types are _functionally equivalent_ to
each other, and that each is _fully functional_.

We'll call two faucet types _functionally equivalent_ iff they can both
represent the same set of water states. And we'll call a faucet type _fully
functional_ iff it can represent all water states, i.e., iff there's a total
surjective function $FaucetState \to WaterState$.

# Water

Instead of dealing with actual values and their correct units, we'll simplify
our model further and use percentages instead.

Water pressure will range from 0% (off, no water running), to 100% (water
running to the faucet's full capacity). Likewise, temperature will range from
0% (cold, the coldest water you can get out of the faucet), to 100% (hot, the
hottest water you can get out of the faucet).

The water pressure and temperature considered are the output pressure and
temperature, that is, what you'd feel right out of the faucet.

The types so far:

 * $Percentage \colon 101$ (or, (roughly) equivalently, $[0, 1]$);
 * $WaterPressure \colon Percentage$;
 * $WaterTemperature \colon Percentage$ -- we'll say the temperature is the
   percentage of hot water in the total amount of water -- this fits just right
   with the definition written in the paragraphs above;
 * $WaterState \colon WaterPressure \times WaterTemperature \approx
   Percentage^2$.

# Faucets

---

## Type _A_

![Type A Faucet](assets/type_a_faucet.png)

This faucet type has two 1D handles, or knobs: one for cold water, and one for
hot water.

In this type of faucet, the water pressure is simply the sum of hot water
pressure and cold water pressure. There's a catch, however: the pressure of hot
and cold water being independent, the maximum hot and cold water pressure must
be half of the maximum faucet water pressure.

And the water temperature is simply the ratio of hot water over the total
amount of water, i.e., hot water pressure over faucet pressure.

This is what we have algebraically:

 * $HotPressure \colon Percentage$;
 * $ColdPressure \colon Percentage$;
 * $FaucetState \colon HotPressure \times ColdPressure$.

$$A \colon FaucetState \to WaterState$$
$$A(h, c) \mapsto ((h + c) / 2, h / (h + c))$$

### Proof

We want to prove that $A$ is surjective. $A$ is surjective iff

$$\forall (p, t) \in WaterState \colon$$
$$\exists (h, c) \in FaucetState \colon$$
$$A(h, c) = (p, t)$$

So, let $(p, t) \in WaterState$; let's go find our $(h, c) \in FaucetState$:

$$
A(h, c) = (p, t)
\iff
\begin{cases}
h = 2pt \\
c = 2(p - pt) \\
\end{cases}
$$

Try $A(2pt, 2(p - pt))$ if you don't believe me. $p - pt$ makes sense because
$p$ and $t$ are percentages:

 1. The product of two percentages is also a percentage;
 2. $p \ge pt \implies p - pt \ge 0$. Alternatively, if you think of a
    percentage as a real number in the range $[0, 1]$, this inequality can be
    expressed as such: $p \ge pt \iff 1 \ge t$.

A less obvious way to write the $c$ is $2p(1 - t)$. Less obvious how to get to
it, but more obvious what it means: the pressure of _non_-hot water, since $t$
is the temperature (with greater meaning hotter).

---

## Type _B_

![Type B Faucet](assets/type_b_faucet.png)

This faucet type has only one 2D handle: one dimension controls the water
pressure, and the other the hot/cold water mixture. In this type of faucet,
both the water pressure and temperature are the faucet's pressure and
temperature, so its mathematical model is _the simplest possible_.

Algebraically:

 * $FaucetPressure \colon Percentage$;
 * $FaucetHotPerc \colon Percentage$ -- to make things easy, we'll say
   that the hot/cold water mixture dimension is to be represented simply as the
   percentage of hot water;
 * $FaucetState \colon FaucetPressure \times FaucetHotPerc \approx WaterState$.

$$B \colon FaucetState \to WaterState$$
$$B(p, h) \mapsto (p, h)$$

### Proof

$B$ clearly is surjective, because it is the identity function.

---

# Faucets as Used by Physical Beings through Mechanical Interaction

Did you notice something with the mathematical models?

I'll give you a hint: it's obvious -- I've even mentioned it already.

Not there yet? Here's another: type _B_.

_Right?!_ It is the _epitome of simplicity!_

Did you notice something else?

Here's a hint: it's not obvious.

Still not obvious? Here: type _A_.

_Right?!_ **Not a damn clue!** But squint harder...

Here goes by analogy: ever tried to move a (computer) cursor on _a single axis_
(vertical, horizontal, doesn't matter) with a (computer) mouse? I'll bet you
have! I'll also bet you've very rarely (and barely) did so satisfactorily, even
if trying very hard! This is easily explained, because while we aim to affect
one dimension only, the controls we have available for use affect more than
that one dimension, _and_ one may simultaneously (and unintentionally) affect
more than that one dimension.

How does this translate to the faucets discussion? Faucets _B_ require you to
set one or two parameters with a single 2D control, that is, set one or two
parameters _simultaneously_. While with faucets _A_ you're only required to set
a single parameter at a time, through a single 1D control for each parameter.

Imagine you're enjoying the current temperature but not so much the current
current. Or maybe you like the current but wish it was a tad hotter. What do
you do if you have a faucet _A_? Just change the thing right away! What do you
do if you have a faucet _B_? _Roll for luck!_

For us, physical beings using faucets through mechanical interaction, it
follows that faucets _A_ are *superior*.

[KISS]: https://en.wikipedia.org/wiki/KISS_principle
