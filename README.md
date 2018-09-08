Trux is a wrapper around Latex for writing reports more easily.  Latex produces beautiful output, but I find it awkward to use and hard to debug so I wrote Trux to make it really easy to create Latex reports.  The main difference from convert-markdown-to-Latex tools, like Pandoc is that Trux has a new, easier way to write mathematics.

# Complete set of usage examples

The intention of this section is that it will contain a minimal example for every single feature of trux, followed by a screenshot of the resulting pdf.

## Hello World

Gives a blank page except for the words "Hello World!".

```
body {
`Hello World!`
}
```

![Hello world](examples/helloWorld.png)

## Document title

Puts a title at the top of the document.

```
title { `Now residence dashwoods she excellent you` }
body { `Now residence dashwoods she excellent you. Shade being under his bed her. Much read on as draw. Blessing for ignorant exercise any yourself unpacked.` }
```

![title](examples/title.png)

## Document author

Puts an author at the top of the document.

```
title { `Not far stuff she think the jokes` }
author { `Robin Slater` }
body { `Is post each that just leaf no. He connection interested so we an sympathize advantages. To said is it shed want do. Occasional middletons everything so to. Have spot part for his quit may. Enable it is square my an regard. Often merit stuff first oh up hills as he.` }
```

![author](examples/author.png)

## Document date

Puts a date at the top of the document.

```
title { `On no twenty spring of in esteem` }
date { `23 July 2018` }
body { `Rank tall boy man them over post now. Off into she bed long fat room. Recommend existence curiosity perfectly favourite get eat she why daughters. Not may too nay busy last song must sell. An newspaper assurance discourse ye certainly. Soon gone game and why many calm have.` }
```

![date](examples/date.png)

## Paragraphs

Make a new paragraph.

```
body {
`Insipidity the sufficient discretion imprudence resolution sir him decisively. Proceed how any engaged visitor. Explained propriety off out perpetual his you. Feel sold off felt nay rose met you. We so entreaties cultivated astonished is. Was sister for few longer mrs sudden talent become. Done may bore quit evil old mile. If likely am of beauty tastes.`
p
`Received the likewise law graceful his. Nor might set along charm now equal green. Pleased yet equally correct colonel not one. Say anxious carried compact conduct sex general nay certain. Mrs for recommend exquisite household eagerness preserved now. My improved honoured he am ecstatic quitting greatest formerly.`
}
```

![paragraph](examples/paragraph.png)

## Italics

Insert italic text.

```
body {
`Normal text followed by ` i `italic text.`
}
```

![italics](examples/italics.png)

## Bold text

Inserts bold text.

```
body {
`Normal text followed by ` b `bold text.`
}
```

![bold](examples/bold.png)


## Insert an image into the document

Inserts an image into the document. First use the word 'image'.  The 'ref23' shown here is a made-up string of characters and numbers.  This can be used to refer to the image in other places in the text. The `0.5` is the width of the image, where `1` is the maximum width between the margins.  The expression in the curly brackets is the caption, and the final `sunflower.jpg` is the name of the image file.

```
body {
`In on announcing if of comparison pianoforte projection. Abroad danger likely regret twenty edward do. Too horrible consider followed may differed age.`

image ref23 `0.5` { `A sunflower` } `sunflower.jpg`

`An rest if more five mr of. Age just her rank met down way. Attended required so in cheerful an. Domestic replying she resolved him for did. Rather in lasted no within no.`
}
```

![image](examples/image.png)

## Tables

Make a simple table.

```
body {
table someref3 { `Cowered outside up because fed` } {
{ { `Among` } { `self-conciously` } { `much` } }
{ { `Other much` } { `less` } { `33.22222` } }
{ { `delicate admonishing` } { `firefly` } { `4933.88` } }
}
}
```

![tables](examples/table.png)

## Mathematics

### Inline mathematics

Inserts mathematics in line with the text.

```
body {
`Pleased yet equally correct colonel ` math { x = 2 } ` rest of know draw fond post as.`
}
```

![bold](examples/inlineMath.png)

### Display-mode mathematics

Inserts mathematics on its own line.  Better for larger expressions and multi-line expressions.

```
body {
`Is branched in my up strictly remember. Songs but chief has ham widow downs. Genius or so up vanity cannot.` 
Math { equation { x = 2 } }
`When be draw drew ye. Defective in do recommend suffering. House it seven in spoil tiled court.` 
}
```

![display-mode mathematics](examples/displayMath.png)

### Fractions

Mathematical fractions.

```
body {
Math { equation { { 3 x ^2 } / { 42 - m c ^3 } } }
}
```

![fractions](examples/fractions.png)

### Powers

Raising things to a power.

```
body {
Math { equation { x ^3 y ^a a ^b b ^ { 3 x + 2 p - 4 z } } }
}
```

![powers](examples/powers.png)

### Greek variables

Use Greek characters as variables.  Only the Greek characters that are easily distinguishable from the Englsh alphabet are allowed.

```
body {
Math { equation { alpha beta gamma Gamma delta Delta epsilon zeta eta theta Theta iota lambda Lambda mu nu xi Xi pi Pi rho sigma Sigma tau upsilon Upsilon phi Phi chi psi Psi omega Omega } }
}
```

![greek variables](examples/greekMath.png)

### Bold variables

These are useful for vectors and matrices.

```
body {
Math { equation { #a #b #c #d #z #Z #omega #Omega #pi #Pi } }
}
```

![bold variables](examples/boldMath.png)

### Ordinary derivatives

```
body {
Math { equation { od x y 1 od x y 3 od a b 5 od omega psi 4 } }
}
```

![ordinary derivatives](examples/ordinaryderivatives.png)

### Partial derivatives

```
body {
Math { equation { pd #eta Sigma 1 pd x y 2 pdMix f 6 x 2 y 3 } }
}
```

![partial derivatives](examples/partialderivatives.png)

### Multiline equations

```
body {
Math {
equation { 3 x + 5 y = 2 = 1 + 1 }
equation { 5 x + 8 y - 4 z ^5 + 530 A ^3 = 3 h + 5 t ^7 }
}
}
```

![multiline mathematics](examples/multilineMath.png)

### Primes

Add a prime after a variable.

```
body {
Math { equation { x ' a ' b ' ' } }
}
```

![primes](examples/primes.png)

### Brackets

All sorts of brackets.

```
body {
Math { equation {
( od a b 1 ) [ { c + 3 x ^3 } / { d q ^4 } ] ( e ) [ f ] curly { { 5 } / { 3 } }
}
}
}
```

![brackets](examples/brackets.png)

### Ordinary text inside equation

```
body {
Math { equation { a + b + c = 3 ` some ordinary text` } }
}
```

![ordinary text in equation](examples/mathText.png)

### Matrices and vectors

```
body {
Math { equation {
[ matrix {
{ { { a } / { 2 x ^3 } } { b } }
{ { c } { d } }
{ { e } { f } } } ]
( matrix {
{ { 1 } }
{ { 0 } } } )
} }
}
```

![matrices](examples/matrices.png)

## Citations

This is for referencing a source.  The details of the reference should be kept ina file called "ref.bib" stored in the same folder as the main trux file.  Its contents is in standard Biblatex format, for example

```
@article{wombat2016,
	author   = {Walther Wombat and Klaus Koala},
	title    = {The true meaning of 42},
	journal  = {Journal of modern skepticism},
	date     = {2016},
	keywords = {trusted},
}
@book{lion2010,
	author       = {Laura Lion and  Gabrielle Giraffe and Carl Capybara},
	title        = {The dangers of asking the wrong question},
	publisher    = {publishing house},
	date         = {2010},
	keywords     = {trusted},
}
@online{wikibook,
	title        = {Generating Bibliographies with biblatex and biber},
	organization = {Wikibooks},
	date         = {2016},
	urldate      = {2016-03-07},
	url          = {https://en.wikibooks.org/wiki/LaTeX/Generating_Bibliographies_with_biblatex_and_biber},
	keywords     = {untrusted},
}
```

taken from [here](https://en.wikibooks.org/wiki/LaTeX/Bibliographies_with_biblatex_and_biber#A_simple_example).

The trux document can refer to these references like this:

```
body {
`At distant inhabit amongst by (` cite wombat2016 `). Appetite welcomed interest the goodness boy not. Estimable education (` cite lion2010 `) for disposing pronounce her. John good plan sent old roof own. Inquietude (` cite wikibook `) saw understood his friendship frequently yet. Nature his marked ham wished.`
}
```

![citation](examples/citation.png)

# Links and urls

There are two ways to do this.

```
body {
`Are sentiments apartments decisively the especially ` url `https://https://www.google.com/maps/place/Timbuktu,+Mali/@16.7713828,-3.0255319,14z/data=!3m1!4b1!4m5!3m4!1s0xe17ce977cbc8733:0x546f01bc8958b7c6!8m2!3d16.7665887!4d-3.0025615` ` alteration. Thrown shy denote ten ladies though ask saw. Or by to he ` link `going` `https://www.google.com/maps/place/Bangladesh/@24.0181187,87.9862164,7.5z/data=!4m5!3m4!1s0x30adaaed80e18ba7:0xf2d28e0c4e1fc6b!8m2!3d23.684994!4d90.356331` ` think order event music. Incommode so intention defective at convinced. Led income months itself and houses you. After nor you leave might share court balls.` 
}
```

![links](examples/links.png)
