{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

{- |

= Type-safe API for Prometheus

A type-safe wrapper around "Prometheus" API from @prometheus-client@
package. In cases when metrics are known in advance, this module can
help with ruling out many potential run time errors. \"Know a metric\"
implies that the name and the labels names can be specified at compile
time.

Also this package provides typed API for labels, so instead of effectively
untyped @Text@-based API you can use different types which also might
prevent from hard-to-debug typos.

-}
module Euler.Events.MetricApi.MetricApi
  (
    -- * Introduction
    -- $intro

    -- * Building metrics
    counter
  , counter'
  , gauge
  , gauge'
  , histogram
  , histogram'
  , (.&)
  , lbl
  , build

    -- * Metrics collection
  , MetricsState
  , Metrics (MNil)
  , (.>)
  , register

    -- * Using metrics
  , (</>)

    -- ** Counters
  , inc
  , add

    -- ** Gauges
  , incGauge
  , setGauge

    -- ** Histograms
  , observe

    -- * Core types
  , MetricSort (..)
  , PromRep
  )
where

{-
NOTES:
 * this file uses doctest, run `cabal run metricapi-doctest` to verify haddock examples

TODO:
 * add help to MetricDef (easy)
 * add histograms support (easy)
 * add more operations
 * implement instances for 6..9-arit6 (easy6
 * fix orphan instance: instance (KnownSymbol s, l ~ s) => IsLabel s (Proxy l)
-}

import Data.Coerce (coerce)
import Data.Kind
import Data.Proxy
import Data.Text (pack)
import Type.Reflection
import GHC.Exts (proxy#)
import GHC.OverloadedLabels
import GHC.TypeLits
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Prometheus as P
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)

{- $setup
>>> :set -XTypeApplications
>>> :set -XOverloadedLabels
>>> :set -XOverloadedStrings
>>> :set -XDataKinds
>>> :set -XTypeOperators
>>> :set -XFlexibleContexts
>>> :set -XGADTs
>>> :set -XTypeFamilies
>>> :set -XAllowAmbiguousTypes
>>> import qualified Prometheus as P
-}

{- $intro

Let's consider a typical workflow based on bare "Prometheus" API from @prometheus-client@ package:

@
λ> myVector1 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests" "")
λ> myVector2 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests" "")
λ> withLabel myVector1 ("GET", "200") incCounter
λ> withLabel myVector2 ("GET", "200") incCounter
λ> withLabel myVector2 ("200", "GET") incCounter
λ> myVector3 <- register $ vector ("name", "name") $ Prometheus.counter (Info "http_requests_" "")
λ> register $ gauge $ Info "" ""
λ> exportMetricsAsText >>= Data.ByteString.Lazy.putStr
# HELP http_requests
# TYPE http_requests counter
http_requests{name="GET",name="200"} 1.0
# HELP http_requests
# TYPE http_requests counter
http_requests{name="GET",name="200"} 1.0
http_requests{name="200",name="GET"} 1.0
# HELP
# TYPE  gauge
 0.0
@

This snippet demonstrates several flaws of the API

* one might register several metrics sharing the same name
* you might give the same names to the labels in vectors, making them ambiguous
* since all label values are just 'Text', one may accidentally flip them when using a labelled metric
* empty names and labels are considered to be acceptable, which does not make any sense

This module solves all the issues mentioned by leveraging type-level Haskell features.

== Example of API's use

Define a __counter__ with name __c0__  without any labels:

>>> c0 = counter #c0 .& build

Let's add a counter __c2__ with two labels, the first being 'Int' and the other being 'Bool':

>>> c2 = counter' #c2 "help" .& lbl @"foo" @Int .& lbl @"bar" @Bool .& build

And a gauge for good measure:

>>> g1 = gauge' #g1 "help" .& lbl @"foo" @Int .& build

Now we are ready to pack all those metrics in a collection:

>>> coll = c2 .> g1 .> c0 .> MNil

This collection can be extended using '.>' operator until it gets registered:

>>> metrics <- register coll

Later we can refer to metrics by their names with '</>' operator. The type
of actions depend on the metric's labels:

>>> inc (metrics </> #c2) 42 True

Notice that we don't have to write any explicit types for metrics anywhere, even to get them
from a collection.

Let's check the results:

>>> P.exportMetricsAsText >>= Data.ByteString.Lazy.putStr
# HELP c2 help
# TYPE c2 counter
c2{foo="42",bar="True"} 1.0
# HELP c0
# TYPE c0 counter
c0 0.0

== On difference between labelled/bare metrics

One peculiarity of labelled metrics in comparison to their unlabelled counterparts is that
when being registered, labelled metrics won't appear in 'exportMetricsAsText` output.
Indeed, in order to make it to the export data such metrics need to get at least one set
of their label values, otherwise it won't make sense. Keep in mind this discrepancy in behaviour.

-}

-- | Types of metrics
data MetricSort = Counter | Gauge | Histogram
  deriving stock (Show, Eq)

-- | Kind synonym for a type-level list of associated pairs from
-- 'Symbol' to 'Type'
type STAssoc = [(Symbol, Type)]

-- type CheckLabelLimit :: STAssoc -> Constraint
type family CheckLabelLimit (types :: STAssoc) :: Constraint where
  CheckLabelLimit types = If ( Length types <=? 15) () (TypeError ('Text "You cannot use more than 15 labels."))

-- type CheckLabelUniqueness :: Symbol -> STAssoc -> Constraint
type family CheckLabelUniqueness (name :: Symbol) (labels :: STAssoc) :: Constraint where
  CheckLabelUniqueness name    ( '(name, _typ) ': tail) = TypeError ('Text "Label names must be unique across a metric.")
  CheckLabelUniqueness another ( '(name, _typ) ': tail) = CheckLabelUniqueness another tail
  CheckLabelUniqueness another '[] = ()

-- type If :: Bool -> Constraint -> Constraint -> Constraint
type family If (cond :: Bool) (the :: Constraint) (els :: Constraint) :: Constraint where
  If 'True  the _ = the
  If 'False _ els = els

type family Length (ls :: [k]) :: Nat where
  Length '[] = 0
  Length (l:ls) = 1 + Length ls

-- type EqSymbol :: Symbol -> Symbol -> Bool
type family EqSymbol (s1 :: Symbol) (s2 :: Symbol) where
  EqSymbol s1 s2 = EqOrd (CmpSymbol s1 s2)

-- type EqOrd :: Ordering -> Bool
type family EqOrd (o :: Ordering) :: Bool where
  EqOrd 'EQ = 'True
  EqOrd _   = 'False

-- type UniqName :: Symbol -> STAssoc -> Constraint
type family UniqName (n :: Symbol) (ns :: STAssoc) :: Constraint where
  UniqName n '[] = ()
  UniqName n ( '(h, _) ': tail) = If (EqSymbol n h)
                                  (TypeError ('Text "Metrics name must be unique in a collection"))
                                  (UniqName n tail)

-- type NotEmpty :: Symbol -> Constraint
type family NotEmpty (s :: Symbol) where
  NotEmpty s = If (EqSymbol "" s) (TypeError ('Text "Empty names/labels are prohibited")) ()

-- | A definition of a metric. Includes its type, name and list of label names and types.
-- type MetricDef :: MetricSort -> Symbol -> STAssoc -> Type
data MetricDef
    (sort :: MetricSort)
    (name :: Symbol)
    (labels :: STAssoc)
    = MetricDef T.Text

-- type PromPrim :: MetricSort -> Type
type family PromPrim s = r | r -> s where
  PromPrim 'Counter = P.Counter
  PromPrim 'Gauge = P.Gauge
  PromPrim 'Histogram = P.Histogram

-- type PrometheusThing :: MetricSort -> Symbol -> Symbol -> STAssoc -> Constraint
class PrometheusThing
  (sort :: MetricSort)
  (name :: Symbol)
  (labels :: STAssoc) where
  data PromRep sort name labels :: Type
  registerMetric :: (P.Info -> P.Metric (PromPrim sort))
    -> MetricDef sort name labels
    -> IO (PromRep sort name labels)
  type PromAction labels :: Type
  runOperation :: (PromPrim sort -> IO ()) -> PromRep sort name labels -> PromAction labels
  -- | A hatch to add some strictness. After the registration this
  -- operation force the evaluation of the metric. The default
  -- implementation does nothing, but 0-ary instance performs
  -- an empty action to do the job.
  dummyOp :: (PromPrim sort -> IO ()) -> PromRep sort name labels -> IO ()
  dummyOp _ _ = pure ()

-- | An auxiliary type class, set correspondence between metric's sort and
-- low-level constructor.
-- type Registrable :: MetricSort -> Constraint
class Registrable sort where
  cons :: P.Info -> P.Metric (PromPrim sort)

instance Registrable 'Counter where
  cons = P.counter

instance Registrable 'Gauge where
  cons = P.gauge

instance Registrable 'Histogram where
  cons = flip P.histogram defaultBuckets

defaultBuckets :: [Double]
defaultBuckets =
 [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 20, 30]

-- | Creates a basic counter definition with empty help
counter :: forall name. NotEmpty name
  => Proxy name
  -> MetricDef 'Counter name '[]
counter p = counter' p $ T.pack ""

-- | Creates a basic counter definition with empty help
counter' :: forall name. NotEmpty name
  => Proxy name
  -> T.Text
  -> MetricDef 'Counter name '[]
counter' _ = MetricDef

-- | Created a basic gauge definition
gauge :: forall name. NotEmpty name
  => Proxy name
  -> MetricDef 'Gauge name '[]
gauge p = gauge' p $ T.pack ""

-- | Created a basic gauge definition
gauge' :: forall name. NotEmpty name
  => Proxy name
  -> T.Text
  -> MetricDef 'Gauge name '[]
gauge' _ = MetricDef

-- | Created a basic histogram definition with empty help
histogram :: forall name. NotEmpty name
  => Proxy name
  -> MetricDef 'Histogram name '[]
histogram p = histogram' p $ T.pack ""

-- | Created a basic histogram definition
histogram' :: forall name. NotEmpty name
  => Proxy name
  -> T.Text
  -> MetricDef 'Histogram name '[]
histogram' _ = MetricDef

type family Snoc (list ::[k]) (elem :: k) :: [k] where
  Snoc '[] e = '[e]
  Snoc (e ': es) elem = e ': (Snoc es elem)

-- | Attaches a label of a given type to a metric of any sort. Up to 9 labels
-- are supported.
lbl
  :: forall
      (label :: Symbol)
      (typ :: Type)
      (types :: STAssoc)
      (sort :: MetricSort)
      (name :: Symbol)
  .  KnownSymbol label
  => CheckLabelLimit types
  => CheckLabelUniqueness label types
  => NotEmpty label
  => MetricDef sort name types
  -> MetricDef sort name (Snoc types '(label, typ))
lbl = coerce

infixl 3 .&
-- | 'Data.Function.&' with higher precedence to use with 'lbl'.
(.&) :: a -> (a -> b) -> b
a .& f = f a
{-# INLINE (.&) #-}

-- | Lazily constructs a metric (calculated by 'PromRep' family) by its definition.
build :: forall sort name labels
       . (PrometheusThing sort name labels
       , Registrable sort)
      => MetricDef sort name labels
      -> PromRep sort name labels
build = unsafePerformIO . (registerMetric cons)

{-------------------------------------------------------------------------------
  Metrics collection
-------------------------------------------------------------------------------}

-- | The state of the metric collection.
data MetricsState
  = Built
  -- ^ a collection has not been registered
  | Registered
  -- ^ a registered collection

-- | Metrics collection, which is effectively a restricted heterogeneous list.
-- It allows only @PromRep sort name labels@ types in it. All @name@s must be
-- unique which is guaranteed by @UniqName@ constraint.
--
-- @:+:@ is polymorphic in @state@, so we don't export it. Instead this module
-- exports '.>' operator which is restricted by @Built@ state.
data Metrics (state :: MetricsState) (map :: STAssoc) where
  MNil  :: Metrics state '[]
  (:+:) :: ( Typeable (PromRep sort name labels)
           , PrometheusThing sort name labels
           , UniqName name as
           , KnownSymbol name
           )
        => PromRep sort name labels
        -> Metrics state as
        -> Metrics state ( '(name, PromRep sort name labels) ': as)
infixr 5 :+:

-- | A cons operator for metric collections5
infixr 4 .>
(.>) :: forall sort name labels as .
      ( Typeable (PromRep sort name labels)
      , PrometheusThing sort name labels
      , UniqName name as
      , KnownSymbol name
      )
      => PromRep sort name labels
      -> Metrics 'Built as
      -> Metrics 'Built ( '(name, PromRep sort name labels) ': as)
(.>) = (:+:)

type family ElemT (s :: Type) (ss :: [Type]) :: Constraint where
  ElemT _ '[] = TypeError ('Text "Not an element!")
  ElemT s (s:t) = ()
  ElemT s (h:t) = ElemT s t

type family ElemST (s :: Symbol) (map :: STAssoc) :: Constraint  where
  ElemST _ '[] = TypeError ('Text "Not an element!")
  ElemST s ( '(s, t) ': r) = ()
  ElemST s ( '(_, _) ': r) = ElemST s r

type family TypeBySym (s :: Symbol) (map :: STAssoc) = r where
  TypeBySym s ( '(s, t) ': r) = t
  TypeBySym s ( '(_, _) ': r) = TypeBySym s r

mMap :: forall state as r
      . (   forall sort name labels
          . PrometheusThing sort name labels
         => SafetyBox (PromRep sort name labels) -> r)
     -> Metrics state as -> [r]
mMap _ MNil = []
mMap f (m :+: bs) = f (SafetyBox m) : mMap f bs

runDummyOp ::
  forall (sort :: MetricSort) (name :: Symbol) (labels :: STAssoc)
                . PrometheusThing sort name labels
               => SafetyBox (PromRep sort name labels) -> IO ()
runDummyOp  (SafetyBox m) = dummyOp @sort @name @labels (\_ -> pure ()) m

-- | An action to register a metric collection.
register :: Metrics 'Built as -> IO (Metrics 'Registered as)
register metrics = do
  _ <- sequence (mMap runDummyOp metrics)
  pure $ coerce metrics

-- | An opaque wrapper for metrics, to protect them from being used directly.
newtype SafetyBox a = SafetyBox a

-- | Extract a metric by its name:
--
-- >>> metricsCollection <- register ((counter' #myCounter "help" .& build) .> MNil)
-- >>> myReadyToUseCounter = metricsCollection </> #myCounter
(</>) :: forall (s :: Symbol) (as :: STAssoc)
       . (ElemST s as, KnownSymbol s)
      => Metrics 'Registered as -> Proxy s -> SafetyBox (TypeBySym s as)
(</>) m _ = go m
  where
    go :: forall (as' :: STAssoc). Metrics 'Registered as'
      -> SafetyBox (TypeBySym s as)
    go MNil = undefined
    go (value :+: rest) = case eqTypeRep (typeOf (Proxy @s)) (typeOf $ metricName value) of
      Just HRefl -> SafetyBox $ unsafeCoerce value
      Nothing  -> go rest

    metricName :: forall sort name labels. PromRep sort name labels
      -> Proxy name
    metricName _ = Proxy @name

-- | @OverloadedLabels@ instance for convenience
instance (KnownSymbol s, l ~ s) => IsLabel s (Proxy l) where
  fromLabel = Proxy @l

{-------------------------------------------------------------------------------
  Working with counters
-------------------------------------------------------------------------------}

-- | Increments a counter. Number and types of arguments depend on the counter
-- definition.
inc :: forall name labels
     . PrometheusThing 'Counter name labels
    => SafetyBox (PromRep 'Counter name labels) -> PromAction labels
inc (SafetyBox m) = runOperation @'Counter @name @labels P.incCounter m

-- | Adds a positive number to a counter. Number and types of arguments depend on the counter
-- definition.
add :: forall name labels
     . PrometheusThing 'Counter name labels
    => SafetyBox (PromRep 'Counter name labels) -> Word -> PromAction labels
add (SafetyBox rep) value = runOperation @'Counter @name @labels
  (flip P.unsafeAddCounter $ fromIntegral value) rep

{-------------------------------------------------------------------------------
  Working with gauge
-------------------------------------------------------------------------------}

-- | Increments a gauge. Number and types of arguments depend on the counter
-- definition.
incGauge :: forall name labels
          . PrometheusThing 'Gauge name labels
         => SafetyBox (PromRep 'Gauge name labels)
         -> PromAction labels
incGauge (SafetyBox m) = runOperation @'Gauge @name @labels P.incGauge m

-- | Set a gauge. Number and types of arguments depend on the counter
-- definition.
setGauge :: forall name labels
          . PrometheusThing 'Gauge name labels
         => SafetyBox (PromRep 'Gauge name labels)
         -> Double
         -> PromAction labels
setGauge (SafetyBox m) value = runOperation @'Gauge @name @labels
  (flip P.setGauge $ value) m

{-------------------------------------------------------------------------------
  Working with histogram
-------------------------------------------------------------------------------}
-- Observe histogram metric
observe :: forall name labels
          . PrometheusThing 'Histogram name labels
         => SafetyBox (PromRep 'Histogram name labels)
         -> Double
         -> PromAction labels
observe (SafetyBox m) value = runOperation @'Histogram @name @labels
  (flip P.observe $ value) m


{-------------------------------------------------------------------------------
  PrometheusThing instances, just repeatative boilerplate
-------------------------------------------------------------------------------}

showT :: (Show a, Typeable a) => a -> T.Text
showT a = case (checkString, checkText, checkBS) of
    (Nothing, Nothing, Nothing) -> pack $ show a
    (Just HRefl, _, _) -> T.pack a
    (_, Just HRefl, _) -> a
    (_, _, Just HRefl) -> TE.decodeUtf8 a
  where
    checkString = eqTypeRep (typeOf a) (typeRep @String)
    checkText = eqTypeRep (typeOf a) (typeRep @T.Text)
    checkBS = eqTypeRep (typeOf a) (typeRep @ByteString)

-- | Bare metric, without any labels
instance (KnownSymbol name)
  => PrometheusThing sort name '[] where
  data instance PromRep sort name '[] = PromRepBare (PromPrim sort)
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register $ con $ P.Info name help)
        >>= pure . PromRepBare
  type PromAction '[] = IO ()
  runOperation op (PromRepBare ref) = op ref
  dummyOp op (PromRepBare ref) = op ref

-- | 1-ary vector
instance (KnownSymbol name, KnownSymbol l1, Show t1, Typeable t1)
  => PrometheusThing sort name '[ '(l1, t1)] where
  data instance PromRep sort name '[ '(l1, t1)] =
    PromRepVec1 (P.Vector (T.Text) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector (pack $ symbolVal' @l1 proxy#)
        $ con $ P.Info name help)
        >>= pure . PromRepVec1
  type PromAction '[ '(l1, t1)] = t1 -> IO ()
  runOperation op (PromRepVec1 ref) v1 = P.withLabel ref (showT v1) op

-- | 2-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         )
  => PrometheusThing sort name '[ '(l1, t1), '(l2, t2)] where
  data instance PromRep sort name '[ '(l1, t1), '(l2, t2)] =
    PromRepVec2 (P.Vector (T.Text, T.Text) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec2
  type PromAction '[ '(l1, t1), '(l2, t2)] = t1 -> t2 -> IO ()
  runOperation op (PromRepVec2 ref) v1 v2 =
    P.withLabel ref
      ( showT v1
      , showT v2
      )
      op

-- | 3-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         )
  => PrometheusThing sort name '[ '(l1, t1), '(l2, t2), '(l3, t3)] where
  data instance PromRep sort name '[ '(l1, t1), '(l2, t2), '(l3, t3)] =
    PromRepVec3 (P.Vector (T.Text, T.Text, T.Text) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec3
  type PromAction '[ '(l1, t1), '(l2, t2), '(l3, t3)] = t1 -> t2 -> t3 -> IO ()
  runOperation op (PromRepVec3 ref) v1 v2 v3 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      )
      op

-- | 4-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         )
  => PrometheusThing sort name '[ '(l1, t1), '(l2, t2), '(l3, t3), '(l4, t4)] where
  data instance PromRep sort name '[ '(l1, t1), '(l2, t2), '(l3, t3), '(l4, t4)] =
    PromRepVec4 (P.Vector (T.Text, T.Text, T.Text, T.Text) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      P.register
        (P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec4
  type PromAction '[ '(l1, t1), '(l2, t2), '(l3, t3), '(l4, t4)]
    = t1 -> t2 -> t3 -> t4 -> IO ()
  runOperation op (PromRepVec4 ref) v1 v2 v3 v4 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      )
      op

-- | 5-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         , KnownSymbol l5, Show t5, Typeable t5
         )
  => PrometheusThing sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    ] where
  data instance PromRep sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    ] =
    PromRepVec5 (P.Vector (T.Text, T.Text, T.Text, T.Text, T.Text) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          , pack $ symbolVal' @l5 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec5
  type PromAction '[ '(l1, t1), '(l2, t2), '(l3, t3), '(l4, t4), '(l5, t5)]
    = t1 -> t2 -> t3 -> t4 -> t5 -> IO ()
  runOperation op (PromRepVec5 ref) v1 v2 v3 v4 v5 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      , showT v5
      )
      op

-- | 6-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         , KnownSymbol l5, Show t5, Typeable t5
         , KnownSymbol l6, Show t6, Typeable t6
         )
  => PrometheusThing sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    ] where
  data instance PromRep sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    ] =
    PromRepVec6 (P.Vector
      ( T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      ) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
      -- help = pack $ symbolVal' @help proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          , pack $ symbolVal' @l5 proxy#
          , pack $ symbolVal' @l6 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec6
  type PromAction
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    ]
    = t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> IO ()
  runOperation op (PromRepVec6 ref) v1 v2 v3 v4 v5 v6 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      , showT v5
      , showT v6
      )
      op

-- | 7-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         , KnownSymbol l5, Show t5, Typeable t5
         , KnownSymbol l6, Show t6, Typeable t6
         , KnownSymbol l7, Show t7, Typeable t7
         )
  => PrometheusThing sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    ] where
  data instance PromRep sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    ] =
    PromRepVec7 (P.Vector
      ( T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      ) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          , pack $ symbolVal' @l5 proxy#
          , pack $ symbolVal' @l6 proxy#
          , pack $ symbolVal' @l7 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec7
  type PromAction
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    ]
    = t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> IO ()
  runOperation op (PromRepVec7 ref) v1 v2 v3 v4 v5 v6 v7 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      , showT v5
      , showT v6
      , showT v7
      )
      op

-- | 8-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         , KnownSymbol l5, Show t5, Typeable t5
         , KnownSymbol l6, Show t6, Typeable t6
         , KnownSymbol l7, Show t7, Typeable t7
         , KnownSymbol l8, Show t8, Typeable t8
         )
  => PrometheusThing sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    ] where
  data instance PromRep sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    ] =
    PromRepVec8 (P.Vector
      ( T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      ) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          , pack $ symbolVal' @l5 proxy#
          , pack $ symbolVal' @l6 proxy#
          , pack $ symbolVal' @l7 proxy#
          , pack $ symbolVal' @l8 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec8
  type PromAction
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    ]
    = t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> IO ()
  runOperation op (PromRepVec8 ref) v1 v2 v3 v4 v5 v6 v7 v8 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      , showT v5
      , showT v6
      , showT v7
      , showT v8
      )
      op

-- | 9-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         , KnownSymbol l5, Show t5, Typeable t5
         , KnownSymbol l6, Show t6, Typeable t6
         , KnownSymbol l7, Show t7, Typeable t7
         , KnownSymbol l8, Show t8, Typeable t8
         , KnownSymbol l9, Show t9, Typeable t9
         )
  => PrometheusThing sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    ] where
  data instance PromRep sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    ] =
    PromRepVec9 (P.Vector
      ( T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      ) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          , pack $ symbolVal' @l5 proxy#
          , pack $ symbolVal' @l6 proxy#
          , pack $ symbolVal' @l7 proxy#
          , pack $ symbolVal' @l8 proxy#
          , pack $ symbolVal' @l9 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec9
  type PromAction
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    ]
    = t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> IO ()
  runOperation op (PromRepVec9 ref) v1 v2 v3 v4 v5 v6 v7 v8 v9 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      , showT v5
      , showT v6
      , showT v7
      , showT v8
      , showT v9
      )
      op

-- | 10-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         , KnownSymbol l5, Show t5, Typeable t5
         , KnownSymbol l6, Show t6, Typeable t6
         , KnownSymbol l7, Show t7, Typeable t7
         , KnownSymbol l8, Show t8, Typeable t8
         , KnownSymbol l9, Show t9, Typeable t9
         , KnownSymbol l10, Show t10, Typeable t10
         )
  => PrometheusThing sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    ] where
  data instance PromRep sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    ] =
    PromRepVec10 (P.Vector
      ( T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      ) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          , pack $ symbolVal' @l5 proxy#
          , pack $ symbolVal' @l6 proxy#
          , pack $ symbolVal' @l7 proxy#
          , pack $ symbolVal' @l8 proxy#
          , pack $ symbolVal' @l9 proxy#
          , pack $ symbolVal' @l10 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec10
  type PromAction
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    ]
    = t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> IO ()
  runOperation op (PromRepVec10 ref) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      , showT v5
      , showT v6
      , showT v7
      , showT v8
      , showT v9
      , showT v10
      )
      op

-- | 11-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         , KnownSymbol l5, Show t5, Typeable t5
         , KnownSymbol l6, Show t6, Typeable t6
         , KnownSymbol l7, Show t7, Typeable t7
         , KnownSymbol l8, Show t8, Typeable t8
         , KnownSymbol l9, Show t9, Typeable t9
         , KnownSymbol l10, Show t10, Typeable t10
         , KnownSymbol l11, Show t11, Typeable t11
         )
  => PrometheusThing sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    , '(l11, t11)
    ] where
  data instance PromRep sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    , '(l11, t11)
    ] =
    PromRepVec11 (P.Vector
      ( T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      ) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          , pack $ symbolVal' @l5 proxy#
          , pack $ symbolVal' @l6 proxy#
          , pack $ symbolVal' @l7 proxy#
          , pack $ symbolVal' @l8 proxy#
          , pack $ symbolVal' @l9 proxy#
          , pack $ symbolVal' @l10 proxy#
          , pack $ symbolVal' @l11 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec11
  type PromAction
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    , '(l11, t11)
    ]
    = t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> IO ()
  runOperation op (PromRepVec11 ref) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      , showT v5
      , showT v6
      , showT v7
      , showT v8
      , showT v9
      , showT v10
      , showT v11
      )
      op

-- | 12-ary vector
instance ( KnownSymbol name
         , KnownSymbol l1, Show t1, Typeable t1
         , KnownSymbol l2, Show t2, Typeable t2
         , KnownSymbol l3, Show t3, Typeable t3
         , KnownSymbol l4, Show t4, Typeable t4
         , KnownSymbol l5, Show t5, Typeable t5
         , KnownSymbol l6, Show t6, Typeable t6
         , KnownSymbol l7, Show t7, Typeable t7
         , KnownSymbol l8, Show t8, Typeable t8
         , KnownSymbol l9, Show t9, Typeable t9
         , KnownSymbol l10, Show t10, Typeable t10
         , KnownSymbol l11, Show t11, Typeable t11
         , KnownSymbol l12, Show t12, Typeable t12
         )
  => PrometheusThing sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    , '(l11, t11)
    , '(l12, t12)
    ] where
  data instance PromRep sort name
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    , '(l11, t11)
    , '(l12, t12)
    ] =
    PromRepVec12 (P.Vector
      ( T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      , T.Text
      ) (PromPrim sort))
  registerMetric con (MetricDef help) =
    let
      name = pack $ symbolVal' @name proxy#
    in
      (P.register
        $ P.vector
          ( pack $ symbolVal' @l1 proxy#
          , pack $ symbolVal' @l2 proxy#
          , pack $ symbolVal' @l3 proxy#
          , pack $ symbolVal' @l4 proxy#
          , pack $ symbolVal' @l5 proxy#
          , pack $ symbolVal' @l6 proxy#
          , pack $ symbolVal' @l7 proxy#
          , pack $ symbolVal' @l8 proxy#
          , pack $ symbolVal' @l9 proxy#
          , pack $ symbolVal' @l10 proxy#
          , pack $ symbolVal' @l11 proxy#
          , pack $ symbolVal' @l12 proxy#
          )
        $ con $ P.Info name help)
        >>= pure . PromRepVec12
  type PromAction
    '[ '(l1, t1)
    , '(l2, t2)
    , '(l3, t3)
    , '(l4, t4)
    , '(l5, t5)
    , '(l6, t6)
    , '(l7, t7)
    , '(l8, t8)
    , '(l9, t9)
    , '(l10, t10)
    , '(l11, t11)
    , '(l12, t12)
    ]
    = t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> t8 -> t9 -> t10 -> t11 -> t12 -> IO ()
  runOperation op (PromRepVec12 ref) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 =
    P.withLabel ref
      ( showT v1
      , showT v2
      , showT v3
      , showT v4
      , showT v5
      , showT v6
      , showT v7
      , showT v8
      , showT v9
      , showT v10
      , showT v11
      , showT v12
      )
      op

