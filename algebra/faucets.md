% Faucets
% siiky
% 2020/02/20

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

# Water

Instead of dealing with actual values and their correct units, we'll simplify
our model to use percentages instead. So the objects will be pairs of
percentages: $100^2$.

Water pressure will range from 0% -- off, no water running --, to 100% -- water
running to the faucet's full capacity. Likewise, temperature will range from 0%
-- cold, the coldest water you can get out of the faucet --, to 100% -- hot,
the hottest water you can get out of the faucet.

The types so far:

 * $Percentage \colon 101$ -- both 0 and 100 are valid values
 * $WaterPressure \colon Percentage$
 * $WaterTemperature\colon Percentage$ -- we'll say the temperature is the
   percentage of hot water in the total amount of water.
 * $WaterState \colon WaterPressure \times WaterTemperature \approx
   Percentage^2$

# Faucets

We'll call two faucet systems functionally the same if they both can represent
the same set of water states. And we'll call a faucet system fully functional
iff it can represent all water states, i.e., iff there's a total surjective
function $FaucetState \to WaterState$.

## Type A Faucets

![Type A Faucet](assets/type_a_faucet.png)

This faucet type has two 1D handles, or knobs: one for cold water, and one for
hot water. In this type of faucet, the water pressure is simply the sum of hot
water pressure and cold water pressure. And the water temperature is simply the
percentage of hot water in the total amount of water, i.e., hot water pressure
over faucet pressure. Algebraically:

 * $HotPressure \colon Percentage$
 * $ColdPressure \colon Percentage$
 * $FaucetState \colon HotPressure \times ColdPressure$

$$A \colon FaucetState \to WaterState$$
$$A(h, c) \mapsto (h + c, h / (h + c))$$

## Type B Faucets

![Type B Faucet](assets/type_b_faucet.png)

This faucet type has only one 2D handle: one dimension controls the water
pressure, the other controls the hot/cold water mixture. In this type of
faucet, both the water pressure and temperature are the faucet's pressure and
temperature.

 * $FaucetPressure \colon Percentage$
 * $FaucetHotPerc \colon Percentage$ -- to make things easy, we'll say
   that the hot/cold water mixture dimension is to be represented simply as the
   percentage of hot water.
 * $FaucetState \colon FaucetPressure \times FaucetHotPerc$

$$B \colon FaucetState \to WaterState$$
$$B(p, h) \mapsto (p, h)$$

[KISS]: https://en.wikipedia.org/wiki/KISS_principle
