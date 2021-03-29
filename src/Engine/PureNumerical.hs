{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances #-}

module Engine.PureNumerical where

-- Trying to compute equilibria in smooth pure strategies using first order conditions + root finding

instance Semigroup Double where
  (<>) = (+)

instance Monoid Double where
  mempty = 0
  mappend = (+)

type Lens s t a b = s -> (a, b -> t)

-- Reverse mode AD is kind of like a lens
type Smooth x y = Lens x x y y

-- Smooth lenses, with a Para(-) construction stuck in too
type SmoothLens a x s y r = (Smooth (a, x) y, Smooth (a, x, r) s)

(>>>>) :: (Monoid a, Monoid x) => SmoothLens a x s y r -> SmoothLens b y r z q -> SmoothLens (a, b) x s z q
(>>>>) (v1, u1) (v2, u2) = (\((a, b), x) -> let y = fst (v1 (a, x))
                                             in (fst (v2 (b, y)),
                                                 \dz -> let (db, dy) = snd (v2 (b, y)) dz
                                                            (da, dx) = snd (v1 (a, x)) dy
                                                        in ((da, db), dx)),
                            \((a, b), x, q) -> let y = fst (v1 (a, x))
                                                   r = fst (u2 (b, y, q))
                                                in (fst (u1 (a, x, r)),
                                                    \ds -> let (da1, dx1, dr) = snd (u1 (a, x, r)) ds
                                                               (db, dy, dq) = snd (u2 (b, y, q)) dr
                                                               (da2, dx2) = snd (v1 (a, x)) dy
                                                            in ((da1 `mappend` da2, db), dx1 `mappend` dx2, dq)))

data SmoothContext p x s y r = SmoothContext (Smooth p x) (Smooth (p, y) r)

data SmoothOG a x s y r = SmoothOG {
  play :: SmoothLens a x s y r,
  eq :: forall p. (Monoid p) => SmoothContext p x s y r -> [Smooth (p, a) Double]}

reindex :: Smooth a b -> SmoothOG b x s y r -> SmoothOG a x s y r
reindex f g = SmoothOG {
  play = (\(a, x) -> let (b, df) = f a
                         (y, dpg) = fst (play g) (b, x)
                      in (y, \dy -> let (db, dx) = dpg dy
                                     in (df db, dx)),
          \(a, x, r) -> let (b, df) = f a
                            (s, dcg) = snd (play g) (b, x, r)
                         in (s, \ds -> let (db, dx, dr) = dcg ds
                                        in (df db, dx, dr))),
  eq = let t e (p, a) = let (b, df) = f a
                            (u, de) = e (p, b)
                         in (u, \du -> let (dp, db) = de du
                                        in (dp, df db))
        in map t . eq g}

decision0 :: (Monoid x) => SmoothOG x () () x Double
decision0 = SmoothOG {
  play = (\(a, ()) -> (a, \da -> (da, ())), \(a, (), u) -> ((), \() -> (mempty, (), 0))),
  eq = \(SmoothContext h k) -> [k]}

decision1 :: (Monoid x, Monoid y) => SmoothOG y x Double (x, y) (Double, Double)
decision1 = SmoothOG {
  play = (\(a, x) -> ((x, a), \(dx, da) -> (da, dx)), \(_, _, (r, _)) -> (r, \dr -> (mempty, mempty, (dr, 0)))),
  eq = \(SmoothContext h k) -> [\(p, y) -> let (x, dh) = h p
                                               ((_, r), dk) = k (p, (x, y))
                                            in (r, \dr -> let (dp, (dx, dy)) = dk (0, dr)
                                                           in (dp `mappend` dh dx, dy))]}

newtype Of x y = Of y

decision1Of :: (Monoid x, Monoid y) => SmoothOG (Of x y) x Double (x, y) (Double, Double)
decision1Of = reindex (\(Of y) -> (y, Of)) decision1

(>>>>>>) :: (Monoid a, Monoid b, Monoid x, Monoid y) => SmoothOG a x s y r -> SmoothOG b y r z q -> SmoothOG (a, b) x s z q
(>>>>>>) g1 g2 = SmoothOG {
  play = play g1 >>>> play g2,
  eq = \(SmoothContext h k) -> let h1 (p, b) = let (z, dh) = h p
                                                in (z, \dx -> (dh dx, mempty))
                                   k1 ((p, b), y) = let (z, dpg) = fst (play g2) (b, y)
                                                        (q, dk) = k (p, z)
                                                        (r, dcg) = snd (play g2) (b, y, q)
                                                     in (r, \dr -> let (db1, dy1, dq) = dcg dr
                                                                       (dp, dz) = dk dq
                                                                       (db2, dy2) = dpg dz
                                                                    in ((dp, db1 `mappend` db2), dy1 `mappend` dy2))
                                   h2 (p, a) = let (x, dh) = h p
                                                   (y, dpg) = fst (play g1) (a, x)
                                                in (y, \dy -> let (da, dx) = dpg dy
                                                               in (dh dx, da))
                                   k2 ((p, a), z) = let (q, dk) = k (p, z)
                                                     in (q, \dq -> let (dp, dz) = dk dq
                                                                    in ((dp, mempty), dz))
                                   braid1 f (p, (a, b)) = (fst (f ((p, b), a)), \de -> let ((dp, db), da) = snd (f ((p, b), a)) de in (dp, (da, db)))
                                   braid2 f (p, (a, b)) = (fst (f ((p, a), b)), \de -> let ((dp, da), db) = snd (f ((p, a), b)) de in (dp, (da, db)))
                                in map braid1 (eq g1 (SmoothContext h1 k1)) ++ map braid2 (eq g2 (SmoothContext h2 k2))}

-- Stackelberg

stackelbergPayoffs :: Smooth (Double, Double) (Double, Double)
stackelbergPayoffs (x, y) = (((1 - x - y)*x, (1 - x - y)*y),
                    \(du, dv) -> (du/(1 - 2*x - y) - dv/y, dv/(1 - x - 2*y) - du/x)) -- reverse derivatives by multivariate chain rule or something (???)

stackelbergCostate :: SmoothOG () (Double, Double) (Double, Double) () ()
stackelbergCostate = SmoothOG {
  play = (\((), (x, y)) -> ((), \() -> ((), (0, 0))),
          \((), (x, y), ()) -> let ((u, v), dp) = stackelbergPayoffs (x, y)
                                in ((u, v), \dudv -> ((), dp dudv, ()))),
  eq = \_ -> []}

stackelberg = (decision0 >>>>>> decision1) >>>>>> stackelbergCostate

instance (Semigroup y) => Semigroup (Of x y) where (<>) (Of a) (Of b) = Of (a <> b)
instance (Monoid y) => Monoid (Of x y) where {mempty = Of mempty; mappend (Of a) (Of b) = Of (mappend a b)}

stackelbergOf = (decision0 >>>>>> decision1Of) >>>>>> stackelbergCostate

trivialContext :: SmoothContext () () () () ()
trivialContext = SmoothContext (\() -> ((), \() -> ())) (\((), ()) -> ((), \() -> ((), ())))

eq1, eq2 :: Smooth ((), ((Double, Double), ())) Double
eq1 = (eq stackelberg trivialContext)!!0
eq2 = (eq stackelberg trivialContext)!!1

