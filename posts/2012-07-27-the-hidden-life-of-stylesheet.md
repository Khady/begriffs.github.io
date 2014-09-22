---
title: The Hidden Life of Stylesheet Preprocessing
---

Like any piece of art, a stylesheet should be indivisible and bold,
but natural. It should arrive to the browser in one elegant file,
a succinct connection between page semantics and presentation.
Preprocessing distances a stylesheet from the Sturm und Drang of
its development, and this distance is necessary for completion and
refinement.

Carpenters remove measurement marks from their finished works.
Painters cover sketch lines. So should CSS authors remove their own
scaffolding from stylesheets. Every "big green button" class, every
"grid-column-2" are so many unseemly nicks and marks. Preprocessors
such as Sass remove these artifacts through the concept of mixins.

Mixins provide modular styles at authoring-time, but are preprocessed
to hide the structure from the client. Consider a "Buy Now" button
-- the HTML should reference only the meaning of the button, not
its presentation.

SCSS for the "Buy Now" button can refer to a notion of being big
and green, but the notion will compile to pure style commands, and
the HTML will reference only the class "purchase."

```scss
button.purchase {
  @include big-green-button
}
```

Along with mixin leitmotifs, Sass allows uniform substitutions with
variables. These authoring variables (such as shared colors) are
another part of the hidden life of preprocessed styles. Designers'
intention of synchronizing certain color values across the web page
is clearly expressed in Sass, but becomes reticent in the generated
CSS, discernable by only repetition itself. The Sass fluidity is
replaced with confidently inscrutable CSS commands.

For network efficiency, CSS should be sent as a single file. However,
in its secret Sass life it can be stored among several files according
to the classification of its rules. Jonathan Snook has made a
five-part classification: base, layout, module, state, and theme.
He describes it in his [SMACSS](http://smacss.com/) methodology.

Beyond its promoting semantic HTML and separating concerns, Sass
allows designers to write their intentions rather than implementations.
Libraries like [Compass](http://compass-style.org/) include mixins
to simplify concepts like [vertical
rhythm](http://compass-style.org/reference/compass/typography/vertical_rhythm/).
Forget tinkering with element padding, just declare that an item
is, for instance, a "leader" or a "trailer" and the library will
add padding appropriate to the size and surroundings of the element.

Finally, the hidden life of preprocessed CSS can include secret
manuscript notes. Authors can add comments to their styles, noting
the ways they can combine, and their variations. For instance a
login form can have invalid fields and required fields. &nbsp;Kyle
Neath believes that the best documentation is living documentation,
and he created a library called [KSS](https://github.com/kneath/kss)
to bring Sass styles to life. KSS parses specially formatted Sass
comments and demonstrate them in a styleguide. Using KSS you can
see your styles in their hidden life, posing for the ways they will
appear in production.
