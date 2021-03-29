{-# LANGUAGE RankNTypes #-}

module Engine.PureNumerical2 where

import Engine.PureNumerical (stackelbergPayoffs)

-- Second attempt

type Smooth x y = x -> (y, y -> x)

type SmoothLens x s y r = (Smooth x y, Smooth (x, r) s)

(>>>>) :: (Monoid x) => SmoothLens x s y r -> SmoothLens y r z q -> SmoothLens x s z q
(>>>>) (v1, u1) (v2, u2) = (\x -> let (y, dv1) = v1 x
                                      (z, dv2) = v2 y
                                   in (z, dv1 . dv2),
                            \(x, q) -> let (y, dv1) = v1 x
                                           (r, du2) = u2 (y, q)
                                           (s, du1) = u1 (x, r)
                                        in (s, \ds -> let (dx, dr) = du1 ds
                                                          (dy, dq) = du2 dr
                                                       in (dx `mappend` dv1 dy, dq)))

data SmoothContext p x s y r = SmoothContext (Smooth p x) (Smooth (p, y) r)

data SmoothOG a x s y r = SmoothOG {
  play :: a -> SmoothLens x s y r,
  eq :: forall p. (Monoid p) => a -> SmoothContext p x s y r -> [Smooth p Double]}

decision0 :: SmoothOG x () () x Double
decision0 = SmoothOG {
  play = \x -> (\() -> (x, \dx -> ()), \((), dx) -> ((), \() -> ((), dx))),
  eq = \x (SmoothContext h k) -> [\p -> let (u, dk) = k (p, x)
                                         in (u, fst . dk)]}

decision1 :: (Monoid x) => SmoothOG (Smooth x y) x () (x, y) Double
decision1 = SmoothOG {
  play = \a -> (\x -> let (y, da) = a x
                       in ((x, y), \(dx1, dy) -> let dx2 = da dy
                                                  in dx1 `mappend` dx2),
                \xr -> ((), \() -> (mempty, 0))),
  eq = \a (SmoothContext h k) -> [\p -> let (u, dk) = k (p, undefined) in undefined]}

{-
decision1 :: (Monoid x) => SmoothOG (Smooth x y) x () y Double
decision1 = SmoothOG {
  play = \a -> (a, \(x, r) -> ((), \() -> (mempty, 0))),
  eq = \a (SmoothContext h k) -> [\p -> let (x, dh) = h p
                                            (y, da) = a x
                                            (u, dk) = k (p, y)
                                            in (u, \du -> let (dp1, dy) = dk du
                                                              dx = da dy
                                                              dp2 = dh dx
                                                           in dp1 `mappend` dp2)]}
-}
(>>>>>>) :: (Monoid x, Monoid y) => SmoothOG a x s y r -> SmoothOG b y r z q -> SmoothOG (a, b) x s z q
(>>>>>>) g1 g2 = SmoothOG {
  play = \(a, b) -> play g1 a >>>> play g2 b,
  eq = \(a, b) (SmoothContext h k) -> let h' p = let (x, dh) = h p
                                                     (y, dpg) = fst (play g1 a) x
                                                  in (y, dh . dpg)
                                          k' (p, y) = let (z, dpg) = fst (play g2 b) y
                                                          (q, dk) = k (p, z)
                                                          (r, dcg) = snd (play g2 b) (y, q)
                                                       in (r, \dr -> let (dy1, dq) = dcg dr
                                                                         (dp, dz) = dk dq
                                                                         dy2 = dpg dz
                                                                      in (dp, dy1 `mappend` dy2))
                                       in eq g1 a (SmoothContext h k') ++ eq g2 b (SmoothContext h' k)}

-- Stackelberg

stackelbergCostate :: SmoothOG () (Double, Double) (Double, Double) () ()
stackelbergCostate = SmoothOG {
  play = \() -> (\_ -> ((), \() -> (0, 0)), \(xy, ()) -> let (uv, dp) = stackelbergPayoffs xy
                                                              in (uv, \duv -> let dxy = dp duv
                                                                                        in (dxy, ()))),
  eq = \() _ -> []}

--stackelberg = (decision0 >>>>>> decision1) >>>>>> stackelbergCostate

trivialContext :: SmoothContext () () () () ()
trivialContext = SmoothContext (\() -> ((), \() -> ())) (\((), ()) -> ((), \() -> ((), ())))
