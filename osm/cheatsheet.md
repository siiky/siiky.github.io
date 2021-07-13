% OpenStreetMap Cheatsheet
% siiky
% 2021/07/12

Here you'll find some personal notes I've gathered, or am still gathering, to
ease and quicken creating and correctly tagging common features on
OpenStreetMap.

# Water Source

# Waste

Features related to [waste processing], such as of trash and recyclables.

## Recycling Container

[`amenity=recycling`]

 ~ Specifies the feature is of some sort of recycling type.

[`recycling_type=container`]

 ~ Specifies the feature is a recycling container, similar to [this one][0] or
   [this one][1].

[`location=*`]

 ~ Specifies where the feature is located, i.e., on the surface, under the
   surface, etc. Two common ones are [`location=overground`] ([pic][0]) and
   [`location=underground`] ([pic][1]). It seems to be optional in the case of
   [`location=overground`], as it seems to me to be implied by
   [`amenity=recycling`].

`recycling:<material>=yes/no`

 ~ A list of materials can be found on [`amenity=recycling`], and as is
   mentioned on that page, if `recycling:X=*` is ommitted, then it is assumed
   to be `no`, as if `recycling:X=no` was present; _unless_ a tag of a
   supergroup of `X` is present and `yes`, in which case it's as if
   `recycling:X=yes` was present.

### The Big Three

By "big three" I mean paper, plastic, and glass:

Paper container ("papelão")

 ~ Alongside paper itself, all kinds of paper-related and paper-derived
   materials, like cardboard, can be deposited.

Plastic container ("embalão")

 ~ All kinds of plastic and metal materials can be deposited. These include
   plastic bottles, plastic bags, tin cans, milk cartons, ...

Glass container ("vidrão")

 ~ All kinds of glass objects, like glass bottles, glass cups, etc, can be
   deposited.

Some of these containers also have batteries containers ("pilhão") next to
them, where you can deposit used batteries.

The correct `recycling:<material>=yes/no` tags, based on [`amenity=recycling`
(PT)], are the following:

```
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
```

**NOTE**: I think styrofoam is allowed in the plastic container. However, I'm
not sure it actually is, nor if it's implied by the tags above.

And in case there's a batteries container, include also the following:

```
recycling:batteries=yes
recycling:car_batteries=no 
```

### Clothes & Footwear

The correct `recycling:<material>=yes/no` tags, based on [`amenity=recycling`
(PT)], are the following:

```
recycling:belts=yes
recycling:clothes=yes
```

**NOTE**: Footwear is accepted, according to the comments, even though there's
no `recycling:footwear=yes` or similar. I guess it's implied by
`recycling:clothes=yes`.

## Trash

Use [`amenity=waste_disposal`] if the feature is a container, or
[`amenity=waste_basket`] if it is a basket.

Additionally, specify the accepted kind of waste with [`waste=*`]:

```
waste=trash
waste=organic
waste=dog_excrement
```

You may also include [`location=*`], and `waste=cigarettes` if there's an
ashtray attached.

[`amenity=recycling` (PT)]: https://wiki.openstreetmap.org/wiki/Pt:Tag:amenity%3Drecycling
[`amenity=recycling`]: https://wiki.openstreetmap.org/wiki/Tag:amenity%3Drecycling
[`amenity=waste_basket`]: https://wiki.openstreetmap.org/wiki/Tag:amenity%3Dwaste_basket
[`amenity=waste_disposal`]: https://wiki.openstreetmap.org/wiki/Tag:amenity%3Dwaste_disposal
[`location=*`]: https://wiki.openstreetmap.org/wiki/Key:location
[`location=overground`]: https://wiki.openstreetmap.org/wiki/Tag:location%3Doverground
[`location=underground`]: https://wiki.openstreetmap.org/wiki/Tag:location%3Dunderground
[`recycling_type=container`]: https://wiki.openstreetmap.org/wiki/Tag:recycling_type%3Dcontainer
[`waste=*`]: https://wiki.openstreetmap.org/wiki/Key:waste
[waste processing]: https://wiki.openstreetmap.org/wiki/Waste_Processing

[0]: https://wiki.openstreetmap.org/wiki/File:Altglas.jpg
[1]: https://wiki.openstreetmap.org/wiki/File:Jt_osm_recycling_underfloor.jpg
