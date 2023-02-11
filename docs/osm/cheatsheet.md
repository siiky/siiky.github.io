# OpenStreetMap Cheat Sheet
siiky
2021/07/12
2022/07/28
en

Here you'll find some personal notes I've gathered, or am still gathering, to
ease and quicken creating and correctly tagging common features on
OpenStreetMap.

If you can't find what you want to map here, have a look at [_How to map
a_](https://wiki.openstreetmap.org/wiki/How_to_map_a).

# Water Source

For sources of water to be used (mainly) by humans. Water sources to used by
animals require a different tag
([`amenity=watering_place`](https://wiki.openstreetmap.org/wiki/Tag:amenity=watering_place)).
Likewise for water to be used to refill RV's deposits and the like
([`amenity=water_point`](https://wiki.openstreetmap.org/wiki/Tag:amenity=water_point)).

* [`drinking_water=*`](https://wiki.openstreetmap.org/wiki/Key:drinking_water):
  after defining the type of water source, you may specify whether the water is
  drinkable. Apart from the obvious `yes` and `no`, `conditional` means "don't
  know" or "drink at your own risk".

* [`drinking_water:legal=*`](https://wiki.openstreetmap.org/wiki/Key:drinking_water:legal)
  (optional): additionally, you may use to specify whether the water source is
  officially (un)safe to drink.

Other possibly interesting tags to take a look at are
[`natural=water`](https://wiki.openstreetmap.org/wiki/Tag:natural=water) and
[`natural=spring`](https://wiki.openstreetmap.org/wiki/Tag:natural=spring).

## Simple Water Source

* [`amenity=drinking_water`](https://wiki.openstreetmap.org/wiki/Tag:amenity=drinking_water):
  specifies the feature is a simple source of water. Check the documentation
  for examples.

* [`drinking_water=*`](https://wiki.openstreetmap.org/wiki/Key:drinking_water)
  (optional; `yes`): specifies whether the water is drinkable. From my
  understanding, together with
  [`amenity=drinking_water`](https://wiki.openstreetmap.org/wiki/Tag:amenity=drinking_water),
  not using this tag is the same as using `drinking_water=yes`.

## Ornamented Water Source

* [`amenity=fountain`](https://wiki.openstreetmap.org/wiki/Tag:amenity=fountain)
  ([PT](https://wiki.openstreetmap.org/wiki/Pt:Tag:amenity=fountain)):
  specifies the feature is a fountain. Check the documentation for examples.

* [`natural=water`](https://wiki.openstreetmap.org/wiki/Tag:natural=water)
  (optional): commonly used together to specify bodies of water next, around,
  under, etc the fountain.

# Waste

Features related to [waste
processing](https://wiki.openstreetmap.org/wiki/Waste_Processing), such as of
trash and recyclables.

## Recycling Container

* [`amenity=recycling`](https://wiki.openstreetmap.org/wiki/Tag:amenity=recycling):
  specifies the feature is of some sort of recycling type.

* [`recycling_type=container`](https://wiki.openstreetmap.org/wiki/Tag:recycling_type=container):
  specifies the feature is a recycling container, similar to [this one][0] or
  [this one][1].

* [`location=*`](https://wiki.openstreetmap.org/wiki/Key:location) (optional):
  specifies where the feature is located, i.e., on the surface, under the
  surface, etc. Two common ones are
  [`location=overground`](https://wiki.openstreetmap.org/wiki/Tag:location=overground)
  ([pic][0]) and
  [`location=underground`](https://wiki.openstreetmap.org/wiki/Tag:location=underground)
  ([pic][1]). It seems to be optional in the case of
  [`location=overground`](https://wiki.openstreetmap.org/wiki/Tag:location=overground),
  as it seems to me to be implied by
  [`amenity=recycling`](https://wiki.openstreetmap.org/wiki/Tag:amenity=recycling).

* `recycling:<material>=yes/no`: a list of materials can be found on
  [`amenity=recycling`](https://wiki.openstreetmap.org/wiki/Tag:amenity=recycling),
  and as is mentioned on that page, if `recycling:X=*` is omitted, then it is
  assumed to be `no`, as if `recycling:X=no` was present; _unless_ a tag of a
  supergroup of `X` is present and `yes`, in which case it's as if
  `recycling:X=yes` was present.

### The Big Three

By "big three" I mean paper, plastic, and glass:

* Paper container ("papelão"): alongside paper itself, all kinds of
  paper-related and paper-derived materials, like cardboard, can be deposited.

* Plastic container ("embalão"): all kinds of plastic and metal materials can
  be deposited. These include plastic bottles, plastic bags, tin cans, milk
  cartons, ...

* Glass container ("vidrão"): all kinds of glass objects, like glass bottles,
  glass cups, etc, can be deposited.

Some of these containers also have batteries containers ("pilhão") next to
them, where you can deposit used batteries.

The correct `recycling:<material>=yes/no` tags, based on [`amenity=recycling`
(PT)](https://wiki.openstreetmap.org/wiki/Pt:Tag:amenity=recycling), are the
following:

    recycling:plastic=yes
    recycling:plastic_bags=yes
    recycling:plastic_bottles=yes
    recycling:plastic_packaging=yes
    recycling:cans=yes
    recycling:PET=yes
    recycling:paper=yes
    recycling:paper_packaging=yes
    recycling:beverage_cartons=yes
    recycling:cardboard=yes
    recycling:cartons=yes
    recycling:magazines=yes
    recycling:newspaper=yes
    recycling:glass=yes
    recycling:glass_bottles=yes

**NOTE**: I think styrofoam is allowed in the plastic container. However, I'm
not sure it actually is, nor if it's implied by the tags above.

And in case there's a batteries basket, include also the following:

    recycling:batteries=yes
    recycling:car_batteries=no

### Clothes & Footwear

The correct `recycling:<material>=yes/no` tags, based on [`amenity=recycling`
(PT)](https://wiki.openstreetmap.org/wiki/Pt:Tag:amenity=recycling), are the
following:

    recycling:belts=yes
    recycling:clothes=yes

**NOTE**: Footwear is accepted, according to the comments, even though there's
no `recycling:footwear=yes` or similar. I guess it's implied by
`recycling:clothes=yes`.

## Trash

Use
[`amenity=waste_disposal`](https://wiki.openstreetmap.org/wiki/Tag:amenity=waste_disposal)
if the feature is a container, or
[`amenity=waste_basket`](https://wiki.openstreetmap.org/wiki/Tag:amenity=waste_basket)
if it is a basket.

Additionally, specify the accepted kind of waste with
[`waste=*`](https://wiki.openstreetmap.org/wiki/Key:waste):

    waste=trash;organic;dog_excrement

If there's an ashtray attached, you can include `cigarettes`:

    waste=trash;organic;dog_excrement;cigarettes

You may also include
[`location=*`](https://wiki.openstreetmap.org/wiki/Key:location).

# Leisure

## Picnic Table

* [`leisure=picnic_table`](https://wiki.openstreetmap.org/wiki/Tag:leisure=picnic_table):
  specifies the feature is picnic table.

* [`covered=*`](https://wiki.openstreetmap.org/wiki/Key:covered) (optional):
  you may include this if it's covered, either completely or partially.

* [`material=*`](https://wiki.openstreetmap.org/wiki/Key:material) (optional):
  specifies the material it's made of.

## Bench

* [`amenity=bench`](https://wiki.openstreetmap.org/wiki/Tag:amenity=bench):
  specifies the feature is a bench.

* [`backrest=*`](https://wiki.openstreetmap.org/wiki/Key:backrest) (optional): specifies whether the bench has or not a backrest.

* [`material=*`](https://wiki.openstreetmap.org/wiki/Key:material) (optional): specifies the material it's made of.

## BBQ Grill

* [`amenity=bbq`](https://wiki.openstreetmap.org/wiki/Tag:amenity=bbq):
  specifies the feature is a BBQ grill.

* [`fuel=*`](https://wiki.openstreetmap.org/wiki/Key:fuel) (optional): the type
  of material you can use as fuel.

* [`covered=*`](https://wiki.openstreetmap.org/wiki/Key:covered) (optional;
  `no`): whether it's covered, either partially or completely.

# Tourism

## Viewpoint (Miradouro)

* [`tourism=viewpoint`](https://wiki.openstreetmap.org/wiki/Tag:tourism=viewpoint):
  specifies the feature is a viewpoint.

* [`name=*`](https://wiki.openstreetmap.org/wiki/Key:name) (optional?):
  specifies the name of the viewpoint.

* [`direction=*`](https://wiki.openstreetmap.org/wiki/Key:direction)
  (optional): the direction of the view; e.g., `0-360` is "all-round".

# Education

## Study Center

[`amenity=prep_school`](https://wiki.openstreetmap.org/wiki/Tag:amenity=prep_school)
seems to be the most appropriate, but for more info see also [_Education
features_](https://wiki.openstreetmap.org/wiki/Education_features).

Some other possible options:

* [`education=centre`](https://wiki.openstreetmap.org/wiki/Tag:education=centre):
  used for specialized areas.
* [`office=tutoring`](https://wiki.openstreetmap.org/wiki/Tag:office=tutoring):
  looks like it's used for an office itself, not where students go to study?

# Popular Restaurant/Cafe/etc chains

## McDonald's

Tags common to all, from
[McDonald's](https://wiki.openstreetmap.org/wiki/Tag:name=McDonald's):

    amenity=fast_food
    brand:wikidata=Q38076
    brand=McDonald's
    cuisine=burger
    name=McDonald's

I believe all McDonald's have takeaway, so:

    takeaway=yes

Some common tags that vary:

    outdoor_seating=yes/no
    smoking=outside
    drive_through=yes/no
    opening_hours=*
    wheelchair=yes/no/...

Don't forget to add `addr:*=*` and `payment:*=*`.

---

Overpass query to find McDonald's restaurants with no `brand`:

    [out:json][timeout:300];
    (
      nwr["name"~"^mcdonald.?s", i]({{bbox}});
      nwr["brand"~"^mcdonald.?s", i]({{bbox}});
      nwr["operator"~"^mcdonald.?s", i]({{bbox}});
    );
    out body;
    >;
    out skel qt;

## Montalegrense

**TODO**: `amenity=cafe`, `amenity=bakery`, `shop=confectionery`, or something else?

## Mixpão

**TODO**: `amenity=cafe`, `amenity=bakery`, `shop=confectionery`, or something else?

# NIF/VAT number

Use [`ref:vatin`](https://wiki.openstreetmap.org/wiki/Key:ref:vatin), e.g. `ref:vatin=PT123456789`.

# Adding turn restrictions

First, in this order, select the "from" way, the intersecting ("via") node, and
the "to" way. Then, "Presets">"Relations">"Turn restriction". Select the
correct restriction (e.g. `no_u_turn`) and hit "New relation". On the lower
left list of objects, select the "from" way and change its role to "from" (and
similarly to the "via" node and the "to" way). Hit "Ok" and you're done.

[0]: https://wiki.openstreetmap.org/wiki/File:Altglas.jpg
[1]: https://wiki.openstreetmap.org/wiki/File:Jt_osm_recycling_underfloor.jpg
