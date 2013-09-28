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

-- * Working with @fclabels@.

{- |
The lens datatype, conveniently called `:->', is an instance of the
"Control.Category" type class: meaning it has a proper identity and
composition. The library has support for automatically deriving labels from
record selectors that start with an underscore.

To illustrate this package, let's take the following two example datatypes.
-}

-- |
-- >{-# LANGUAGE TemplateHaskell, TypeOperators #-}
-- >import Control.Category
-- >import Data.Label
-- >import Prelude hiding ((.), id)
-- >
-- >data Person = Person
-- >  { _name   :: String
-- >  , _age    :: Int
-- >  , _place  :: Place
-- >  } deriving Show
-- >
-- >data Place = Place
-- >  { _city
-- >  , _country
-- >  , _continent :: String
-- >  } deriving Show

{- |
Both datatypes are record types with all the labels prefixed with an
underscore. This underscore is an indication for our Template Haskell code to
derive lenses for these fields. Deriving lenses can be done with this simple
one-liner:

>mkLabels [''Person, ''Place]

For all labels a lens will created.

Now let's look at this example. This 71 year old fellow, my neighbour called
Jan, didn't mind using him as an example:

>jan :: Person
>jan = Person "Jan" 71 (Place "Utrecht" "The Netherlands" "Europe")

When we want to be sure Jan is really as old as he claims we can use the `get`
function to get the age out as an integer:

>hisAge :: Int
>hisAge = get age jan

Consider he now wants to move to Amsterdam: what better place to spend your old
days. Using composition we can change the city value deep inside the structure:

>moveToAmsterdam :: Person -> Person
>moveToAmsterdam = set (city . place) "Amsterdam"

And now:

>ghci> moveToAmsterdam jan
>Person "Jan" 71 (Place "Amsterdam" "The Netherlands" "Europe")

Composition is done using the @(`.`)@ operator which is part of the
"Control.Category" module. Make sure to import this module and hide the default
@(`.`)@, `id` function from the Haskell "Prelude".

-}

-- * Total monomorphic lenses.

  (:->)
, lens
, get
, set
, modify

-- * Vertical composition using @Applicative@.

{- |

Now, because Jan is an old guy, moving to another city is not a very easy task,
this really takes a while. It will probably take no less than two years before
he will actually be settled. To reflect this change it might be useful to have
a first class view on the `Person` datatype that only reveals the age and
city. This can be done by using a neat `Applicative` functor instance:

>import Control.Applicative

>(fstL, sndL) = $(getLabel ''(,))

>ageAndCity :: Person :-> (Int, String)
>ageAndCity = point $
>  (,) <$> fstL >- age
>      <*> sndL >- city . place

Because the applicative type class on its own is not capable of expressing
bidirectional relations, which we need for our lenses, the actual instance is
defined for an internal helper structure called `Point`. Points are a more
general than lenses. As you can see above, the `point` function has to be
used to convert a `Point` back into a `Lens`. The (`>-`) operator is used to
indicate which partial destructor to use per arm of the applicative
composition.

Now that we have an appropriate age+city view on the `Person` datatype (which
is itself a lens again), we can use the `modify` function to make Jan move to
Amsterdam over exactly two years:

>moveToAmsterdamOverTwoYears :: Person -> Person
>moveToAmsterdamOverTwoYears = modify ageAndCity (\(a, _) -> (a+2, "Amsterdam"))

>ghci> moveToAmsterdamOverTwoYears jan
>Person "Jan" 73 True (Place "Amsterdam" "The Netherlands" "Europe")

-}

, point
, (>-)

-- * Working with isomorphisms.
--
-- | This package contains an isomorphisms datatype that encodes bidirectional
-- functions, or better bidirectional categories. Just like lenses,
-- isomorphisms can be composed using the `Category` type class. Isomorphisms
-- can be used to change the type of a lens. Every isomorphism can be lifted
-- into a lens.
--
-- For example, when we want to treat the age of a person as a string we can do
-- the following:
--
-- > ageAsString :: Person :-> String
-- > ageAsString = iso (Iso show read) . age

, Isomorphism (..)
, inv
, iso

-- * Derive labels using Template Haskell.
--
-- | Template Haskell functions for automatically generating labels for
-- algebraic datatypes, newtypes and GADTs. There are two basic modes of label
-- generation, the `mkLabels` family of functions create labels (and optionally
-- type signatures) in scope as top level funtions, the `getLabel` family of
-- funtions create labels as expressions that can be named and typed manually.
--
-- In the case of multi-constructor datatypes some fields might not always be
-- available and the derived labels will be partial. Partial labels are
-- provided with an additional type context that forces them to be only usable
-- in the `Partial' or `Failing` context.
--
-- More derivation functions can be found in "Data.Label.Derive".

, mkLabel
, mkLabels
, getLabel
)
where

import Data.Label.Point (Isomorphism(..), inv)
import Data.Label.Poly (point, (>-))
import Data.Label.Mono (iso, (:->))
import Data.Label.Derive

import qualified Data.Label.Mono as Mono

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}

-------------------------------------------------------------------------------

-- | Create a total lens from a getter and a modifier.
--
-- We expect the following law to hold:
--
-- > get l (modify l m f) == m (get l f)

lens :: (f -> a)              -- ^ Getter.
     -> ((a -> a) -> f -> f)  -- ^ Modifier.
     -> f :-> a
lens g s = Mono.lens g (uncurry s)

-- | Get the getter function from a lens.

get :: (f :-> a) -> f -> a
get = Mono.get

-- | Get the modifier function from a lens.

modify :: f :-> a -> (a -> a) -> f -> f
modify = curry . Mono.modify

-- | Get the setter function from a lens.

set :: (f :-> a) -> a -> f -> f
set = curry . Mono.set

