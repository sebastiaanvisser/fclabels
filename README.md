# fclabels: first class accessor labels

This package provides first class labels that can act as bidirectional record
fields. The labels can be derived automatically using Template Haskell which
means you don't have to write any boilerplate yourself. The labels are
implemented as _lenses_ and are fully composable. Lenses can be used to _get_,
_set_ and _modify_ parts of a data type in a consistent way.

See `Data.Label` for an introductory explanation.

### Total and partial lenses

Internally lenses do not used Haskell functions directly, but are implemented
as categories. Categories allow the lenses to be run in custom computational
contexts. This approach allows us to make partial lenses that point to fields
of multi-constructor datatypes in an elegant way.

See `Data.Label.Partial` for the use of partial labels.

### Monomorphic and polymorphic lenses

We have both polymorphic and monomorphic lenses. Polymorphic lenses allow
updates that change the type. The types of polymorphic lenses are slightly more
verbose than their monomorphic counterparts, but their usage is similar.
Because monomorphic lenses are built by restricting the types of polymorphic
lenses they are essentially the same and can be freely composed with eachother.

See `Data.Label.Mono` and `Data.Label.Poly` for the difference between
polymorphic and monomorphic lenses.

### Using fclabels

To simplify working with labels we supply both a set of labels for Haskell's
base types, like lists, tuples, Maybe and Either, and we supply a set of
combinators for working with labels for values in the Reader and State monad.

See `Data.Label.Base` and `Data.Label.Monadic` for more information.

On Hackage: http://hackage.haskell.org/package/fclabels

Introduction: http://fvisser.nl/post/2013/okt/1/fclabels-2.0.html

