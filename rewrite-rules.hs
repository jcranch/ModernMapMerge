-- Rewrite rules
--
-- Cannot be used in this form, for two reasons:
--  - some will not typecheck since the RHS requires a more restrictive typeclass
--  - cannot use rules whose head is a typeclass method
-- We can refactor them into rules which rewrite WhenMissing instances
--
-- There are a lot; one per pair of composable methods. We list them
-- in a systematic order, ordered by
-- (1) left-hand method (Functor, Foldable, Traversable, Filterable,
--     Witherable),
-- (2) right-hand method (Functor, Traversable, Filterable,
--     Witherable),
-- (3) left-hand unindexed then indexed,
-- (4) right-hand unindexed then indexed.

-- Functor . Functor
{-# RULES
  "fmap.fmap"           forall f g a.  fmap g (fmap f a) = fmap (g . f) a;
  #-}

-- Functor . FunctorWithIndex
{-# RULES
  "fmap.imap"           forall f g a.  fmap g (imap f a) = imap (\i -> g . f i) a;
  #-}

-- Functor . Traversable
{-# RULES
  "fmap.traverse"       forall f g a.  fmap (fmap g) (traverse f a) = traverse (fmap g . f) a;
  #-}

-- Functor . TraversableWithIndex
{-# RULES
  "fmap.itraverse"      forall f g a.  fmap (fmap g) (itraverse f a) = itraverse (\i -> fmap g . f i) a;
  #-}

-- Functor . Filterable
{-# RULES
  "fmap.filter"         forall f g a.  fmap g (filter f a) = mapMaybe (\x -> if f x then Just (g x) else Nothing) a;
  "fmap.mapMaybe"       forall f g a.  fmap g (mapMaybe f a) = mapMaybe (fmap g . f) a;
  "fmap.catMaybes"      forall g a.    fmap g (catMaybes a) = mapMaybe (fmap g) a
  #-}

-- Functor . FilterableWithIndex
{-# RULES
  "fmap.ifilter"        forall f g a.  fmap g (ifilter f a) = imapMaybe (\i x -> if f i x then Just (g x) else Nothing) a;
  "fmap.imapMaybe"      forall f g a.  fmap g (imapMaybe f a) = imapMaybe (\i -> fmap g . f i) a;
  #-}

-- Functor . Witherable
{-# RULES
  "fmap.wither"         forall f g a.  fmap (fmap g) (wither f a) = wither (fmap (fmap g) . f) a;
  #-}

-- Functor . WitherableWithIndex
{-# RULES
  "fmap.iwither"        forall f g a.  fmap (fmap g) (iwither f a) = iwither (\i -> fmap (fmap g) . f i) a;
  #-}

-- FunctorWithIndex . Functor
{-# RULES
  "imap.fmap"           forall f g a.  imap g (fmap f a) = imap (\i -> g i . f) a;
  #-}

-- FunctorWithIndex . FunctorWithIndex
{-# RULES
  "imap.imap"           forall f g a.  imap g (imap f a) = imap (\i -> g i . f i) a;
  #-}

-- FunctorWithIndex . Traversable
{-# RULES
  "imap.traverse"       forall f g a.  imap (fmap g) (traverse f a) = itraverse (\i -> fmap (g i) . f) a;
  #-}

-- FunctorWithIndex . TraversableWithIndex
{-# RULES
  "imap.itraverse"      forall f g a.  imap (fmap g) (itraverse f a) = itraverse (\i -> fmap (g i) . f i) a;
  #-}

-- FunctorWithIndex . Filterable
{-# RULES
  "imap.filter"         forall f g a.  imap g (filter f a) = imapMaybe (\i x -> if f x then Just (g i x) else Nothing) a;
  "imap.mapMaybe"       forall f g a.  imap g (mapMaybe f a) = imapMaybe (\i -> fmap (g i) . f) a;
  "imap.catMaybes"      forall g a.    imap g (catMaybes a) = imapMaybe (\i -> fmap (g i)) a;
  #-}

-- FunctorWithIndex . FilterableWithIndex
{-# RULES
  "imap.ifilter"        forall f g a.  imap g (ifilter f a) = imapMaybe (\i x -> if f i x then Just (g i x) else Nothing) a;
  "imap.imapMaybe"      forall f g a.  imap g (imapMaybe f a) = imapMaybe (\i -> fmap (g i) . f i) a;
  #-}

-- FunctorWithIndex . Witherable
{-# RULES
  "imap.wither"         forall f g a.  imap (fmap g) (wither f a) = iwither (\i -> fmap (fmap (g i)) . f) a;
  #-}

-- FunctorWithIndex . WitherableWithIndex
{-# RULES
  "imap.iwither"        forall f g a.  fmap (imap g) (iwither f a) = iwither (\i -> fmap (fmap (g i)) . f i) a;
  #-}

-- Foldable . Functor
{-# RULES
  "foldMap.fmap"        forall f g a.  foldMap g (fmap f a) = foldMap (g . f) a
  #-}

-- Foldable . FunctorWithIndex
{-# RULES
  "foldMap.imap"        forall f g a. foldMap g (imap f a) = ifoldMap (\i -> g . f i) a;
  #-}

{-
-- Foldable . Traversable
--     fmap (foldMap g) (traverse f a)
--       [foldMap as traverse]
--   = fmap (getConst . traverse (Const . g)) (traverse f a)
--       [functoriality]
--   = fmap getConst (fmap (traverse (Const . g)) (traverse f a))
--       [traverse.traverse]
--   = fmap getConst (getCompose (traverse (Compose . fmap (Const . g) . f) a))
--       [naturality?]
--   = getCompose (fmap (fmap getConst) (traverse (Compose . fmap (Const . g) . f) a))
--       [fmap.traverse]
--   = getCompose (traverse (fmap getConst . Compose . fmap (Const . g) . f) a)
--       ?
--   = getCompose (traverse (fmap getConst . Compose . fmap (Const . g) . f) a)
{-# RULES
  "foldMap.traverse"    forall f g a.  fmap (foldMap g) (traverse f a) = traverse () a
  #-}
-}

-- Foldable . Filterable
{-# RULES
  "foldMap.filter"      forall f g a.  foldMap g (filter f a) = foldMap (\x -> if f x then g x else mempty) a;
  "foldMap.mapMaybe"    forall f g a.  foldMap g (mapMaybe f a) = foldMap (\x -> case f x of {Just y -> g y; Nothing -> mempty}) a;
  "foldMap.catMaybes"   forall g a.    foldMap g (catMaybes a) = foldMap (\x -> case x of {Just y -> y; Nothing -> mempty}) a;
  #-}

-- Traversable . Traversable
{-# RULES
  "traverse.traverse"   forall f g a.  fmap (traverse g) (traverse f a) = getCompose (traverse (Compose . fmap g . f) a);
  #-}

-- Filterable . Functor
{-# RULES
  "filter.fmap"         forall f g a.  filter g (fmap f a) = mapMaybe ((\x -> if g x then Just x else Nothing) . f) a;
  "mapMaybe.fmap"       forall f g a.  mapMaybe g (fmap f a) = mapMaybe (g . f) a;
  "catMaybes.fmap"      forall f a.    catMaybes (fmap f a) = mapMaybe f a;
  #-}

{-
-- Filterable . Filterable
{-# RULES
  "filter.filter"       forall f g a.  filter g (filter f a) = filter (liftA2 (&&) f g) a;
  "filter.mapMaybe"     forall f g a.  filter g (mapMaybe f a) = _;
  "mapMaybe.mapMaybe"   forall f g a.  mapMaybe g (mapMaybe f a) = _;
  "mapMaybe.filter"     forall f g a.  mapMaybe g (filter f a) = _;
  #-}
-}

-- Witherable . Witherable

{-
-- WitherableWithIndex . WitherableWithIndex
{-# RULES
  "iwither.iwither"     forall f g a.  fmap (iwither g) . iwither f = getCompose . iwither (Compose . _)
  #-}
-}
