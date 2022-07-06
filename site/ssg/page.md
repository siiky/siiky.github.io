% Static Site Generator
% siiky
% 2017/03/06

**EDIT**: [WIP](https://github.com/siiky/ssg).

Simple set and forget static site generator

 * Markdown -> HTML (keep overall look of markdown)
 * Filesystem structure traversing (`tree` like)

## Markdown -> HTML

 * **Headers**
 * _Code_ (blocks and inline)
     * Blocks
     * Inline
 * URLs (inline and by reference)
     * Inline
     * Reference
 * Images (inline and by reference)
     * Inline
     * Reference

### Headers

From:

```markdown
# H1
## H2
### H3
#### H4
##### H5
###### H6
```

to:

```html
<H1># H1</H1>
<H2>## H2</H2>
<H3>### H3</H3>
<H4>#### H4</H4>
<H5>##### H5</H5>
<H6>###### H6</H6>
```

### Code

#### Blocks

From:

```markdown
```some\_lang\_here
some code here
```
```

to:

```html
```some\_lang\_here<CODE>
some code here
</CODE>```
```

#### Inline

From:

```markdown
`some code here`
```

to:

```html
`<CODE>some code here</CODE>`
```

### URLs

#### Inline

From:

```markdown
[some text](some_url)
```

to:

```html
<A HREF="some_url">some text</A>
```

#### Reference

From:

```markdown
[some text][ref]
[ref]: some_url
```

to:

```html
<A HREF="some_url">some text</A>
```

### Images

#### Inline

From:

```markdown
![some text](some_url)
```

to:

```html
<IMG SRC="some_url" ALT="some text">
```

#### Reference

From:

```markdown
![some text][ref]
[ref]: some_url
```

to:

```html
<IMG SRC="some_url" ALT="some text">
```

## Filesystem Structure Traversing

From:

```filesystem
.
├── words
│  ├── slpod.md
│  └── ssg.md
└── index.md
```

to:

```filesystem
.
├── words
│  ├── slpod.md
│  ├── slpod.html
│  ├── ssg.md
│  └── ssg.html
├── index.md
└── index.html
```

Inside `index.html`:

```html
.
└── words
├── <A href="words/slpod.html">slpod.md</A>
└── <A href="words/ssg.html">ssg.md</A>
```

```scheme
(print "Hello, World!")
```

![Jose Cid](assets/josecid.jpg)
