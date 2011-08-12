{-# LANGUAGE TypeOperators #-}
{- |
This package provides first class labels that can act as bidirectional record
fields. The labels can be derived automatically using Template Haskell which
means you don't have to write any boilerplate yourself. The labels are
implemented as lenses and are fully composable. Labels can be used to /get/,
/set/ and /modify/ parts of a datatype in a consistent way.
-}

module Data.Label
(

-- * Pure lenses.

  (:->)
, lens
, get
, set
, modify

-- * Working with @fclabels@.

{- |
The lens datatype, conveniently called `:->', is an instance of the
"Control.Category" type class: meaning it has a proper identity and
composition. The library has support for automatically deriving labels from
record selectors that start with an underscore.

To illustrate this package, let's take the following two example datatypes.

> data Person = Person
>   { _name   :: String
>   , _age    :: Int
>   , _isMale :: Bool
>   , _place  :: Place
>   }

> data Place = Place
>   { _city
>   , _country
>   , _continent :: String
>   }

Both datatypes are record types with all the labels prefixed with an
underscore. This underscore is an indication for our Template Haskell code to
derive lenses for these fields. Deriving lenses can be done with this simple
one-liner:

> $(mkLabels [''Person, ''Place])

For all labels a lens will created.

Now let's look at this example. This 71 year old fellow, my neighbour called
Jan, didn't mind using him as an example:

> jan :: Person
> jan = Person "Jan" 71 True (Place "Utrecht" "The Netherlands" "Europe")

When we want to be sure Jan is really as old as he claims we can use the `get`
function to get the age out as an integer:

> hisAge :: Int
> hisAge = get age jan

Consider he now wants to move to Amsterdam: what better place to spend your old
days. Using composition we can change the city value deep inside the structure:

> moveToAmsterdam :: Person -> Person
> moveToAmsterdam = set (city . place) "Amsterdam"

And now:

> ghci> moveToAmsterdam jan
> Person "Jan" 71 True (Place "Amsterdam" "The Netherlands" "Europe")

Composition is done using the @(`.`)@ operator which is part of the
"Control.Category" module. Make sure to import this module and hide the default
@(`.`)@, `id` function from the Haskell "Prelude".

-}

-- * Views using @Applicative@.

{- |

Now, because Jan is an old guy, moving to another city is not a very easy task,
this really takes a while. It will probably take no less than two years before
he will actually be settled. To reflect this change it might be useful to have
a first class view on the `Person` datatype that only reveals the age and
city.  This can be done by using a neat `Applicative` functor instance:

> ageAndCity :: Person :-> (Int, String)
> ageAndCity = Lens $ (,) <$> fst `for` age <*> snd `for` city . place

Because the applicative type class on its own is not very capable of expressing
bidirectional relations, which we need for our lenses, the actual instance is
defined for an internal helper structure called `Point`. Points are a bit more
general than lenses. As you can see above, the `Label` constructor has to be
used to convert a `Point` back into a `Label`. The `for` function must be used
to indicate which partial destructor to use for which lens in the applicative
composition.

Now that we have an appropriate age+city view on the `Person` datatype (which
is itself a lens again), we can use the `modify` function to make Jan move to
Amsterdam over exactly two years:

> moveToAmsterdamOverTwoYears :: Person -> Person
> moveToAmsterdamOverTwoYears = modify ageAndCity (\(a, b) -> (a+2, "Amsterdam"))

> ghci> moveToAmsterdamOverTwoYears jan
> Person "Jan" 73 True (Place "Amsterdam" "The Netherlands" "Europe")

-}

-- * Working with bijections and isomorphisms.
-- 
-- | This package contains a bijection datatype that encodes bidirectional
-- functions. Just like lenses, bijections can be composed using the
-- "Control.Category" type class. Bijections can be used to change the type of
-- a lens. The `Iso` type class, which can be seen as a bidirectional functor,
-- can be used to apply lenses to lenses.
-- 
-- For example, when we want to treat the age of a person as a string we can do
-- the following:
-- 
-- > ageAsString :: Person :-> String
-- > ageAsString :: Bij show read % age

, Bijection (..)
, Iso (..)
, for

-- * Derive labels using Template Haskell.
--
-- | We can either derive labels with or without type signatures.

, mkLabels
, mkLabelsNoTypes
)
where

import Data.Label.Abstract (Bijection(..), Iso(..), for)
import Data.Label.Pure
import Data.Label.Derive

