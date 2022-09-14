module OpenGames

import Data.List
import Data.Vect

infixr 8 >>>>
infixl 7 &&&, &&&&
infixl 6 +++, ++++

infixr 5 //, \\

%hide Prelude.either

either : (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left a) = f a
either f g (Right b) = g b

interface Optic (0 o : Type -> Type -> Type -> Type -> Type) where
  lens : (s -> a) -> (s -> b -> t) -> o s t a b
  (>>>>) : o s t a b -> o a b p q -> o s t p q
  (&&&&) : o s1 t1 a1 b1 -> o s2 t2 a2 b2 -> o (s1, s2) (t1, t2) (a1, a2) (b1, b2)
  (++++) : o s1 t a1 b -> o s2 t a2 b -> o (Either s1 s2) t (Either a1 a2) b

identity : (Optic o) => o s t s t
identity = lens id (flip const)

interface (Optic o) => Context (0 c, o : Type -> Type -> Type -> Type -> Type) where
  cmap : o s1 t1 s2 t2 -> o a1 b1 a2 b2 -> c s1 t1 a2 b2 -> c s2 t2 a1 b1
  (//) : o s1 t1 a1 b1 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s2 t2 a2 b2
  (\\) : o s2 t2 a2 b2 -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> c s1 t1 a1 b1
  both : o s1 t1 a1 b1 -> o s2 t2 a2 b2 
      -> c (s1, s2) (t1, t2) (a1, a2) (b1, b2) -> (c s2 t2 a2 b2, c s1 t1 a1 b1)
  both g1 g2 c = (g1 // c, g2 \\ c)
 

data TriState a b = One a | Two b | Both a b

elimTri : Monoid c => (a -> c) -> (b -> c) -> TriState a b -> c
elimTri f g (One x) = f x
elimTri f g (Two y) = g y
elimTri f g (Both x y) = f x <+> g y

interface ContextAdd (0 c : Type -> Type -> Type -> Type -> Type) where
  match : c (Either x1 x2) s (Either y1 y2) r
       -> TriState (c x1 s y1 r) (c x2 s y2 r)

data TypeList : List Type -> Type where
  Nil : TypeList []
  (::) : (ty : Type) -> TypeList ts -> TypeList (ty :: ts)

Tuplelize : List Type -> Type
Tuplelize [] = ()
Tuplelize [x] = x
Tuplelize (x :: xs) = Pair x (Tuplelize xs)

record OpenGame (o, c : Type -> Type -> Type -> Type -> Type)
                (a : List Type)
                (x, s, y, r : Type)
                (f : TypeList a -> c x s y r -> List Type)
                where
  constructor MkGame
  play : TypeList a -> o x s y r
  evaluate : (ls : TypeList a) -> (v : c x s y r) -> TypeList (f ls v)

(++) : TypeList xs -> TypeList ys -> TypeList (xs ++ ys)
(++) [] y = y
(++) (ty :: x) y = ty :: (x ++ y)

split : {xs : _} -> TypeList (xs ++ ys) -> (TypeList xs, TypeList ys)
split x {xs = []} = ([], x)
split (y :: ts) {xs = (y :: xs)} =
  case OpenGames.split ts of
       (xs', ys') => (y :: xs', ys')

left : {xs : _} -> TypeList (xs ++ ys) -> TypeList xs
left = fst . split

right : {xs : _} -> TypeList (xs ++ ys) -> TypeList ys
right = snd . split

choice : (select : Bool) -> (b : TypeList ks) -> (b' : TypeList ks')
      -> TypeList (if select then ks else ks')
choice True b b' = b
choice False b b' = b'

TensorTy : {0 c : Type -> Type -> Type -> Type -> Type} ->
           (c x s y r -> List Type) ->
	         (c x' s' y' r' -> List Type) ->
           (c x s y r, c x' s' y' r') ->
	         List Type
TensorTy fl fr (l, r) = fl l ++ fr r

SequenceTy : Optic o => Context c o
         => o x s y r -> o y r z q
	 -> (c x s y r -> List Type)
	 -> (c y r z q -> List Type)
	 -> (c x s z q -> List Type)
SequenceTy o1 o2 f g w = f (cmap {o} identity o2 w)
                      ++ g (cmap {o} o1 identity w)

BigSeq : {a, a' : List Type}
     -> Optic o => Context c o
   	 => (b : TypeList a -> c x s y r ->  List Type)
   	 -> (b' : TypeList a' -> c y r z q ->  List Type)
     -> OpenGame o c a x s y r b
     -> OpenGame o c a' y r z q b'
   	 -> (TypeList (a ++ a') -> c x s z q ->  List Type)
BigSeq b b' g1 g2 x w = b  (fst (split x)) (cmap {o} identity (g2.play (snd (split x))) w)
                     ++ b' (snd (split x)) (cmap {o} (g1.play (fst (split x))) identity w)

-- Sequence operator
(>>>) : {a, a' : List Type } -> (Optic o, Context c o)
      => {x, s, y, r, z, q : Type}
      -> {b : TypeList a -> c x s y r ->  List Type}
      -> {b' : TypeList a' -> c y r z q ->  List Type}
      -> (g1 : OpenGame o c a x s y r b) -> (g2 : OpenGame o c a' y r z q b')
      -> OpenGame o c (a ++ a')
                      x s z q
                      (BigSeq {c} {o} b b' g1 g2)
(>>>) g1 g2 =
  MkGame
    (\tl => case split tl of (left, right) => g1.play left >>>> g2.play right)
    (\tl, body => g1.evaluate (fst (split tl)) (cmap {c} {o} identity (g2.play (snd (split tl))) body)
               ++ g2.evaluate (snd (split tl)) (cmap {c} {o} (g1.play (fst (split tl))) identity body))

0 ChoiceTy : {a, a' : List Type}
     -> {b : TypeList a -> c x1 s y1 r -> List Type}
     -> {b' : TypeList a' -> c x2 s y2 r -> List Type}
     -> Optic o => Context c o => ContextAdd c
     => (g1 : OpenGame o c a x1 s y1 r b) -> (g2 : OpenGame o c a' x2 s y2 r b')
   	 -> (TypeList (a ++ a') -> c (Either x1 x2) s (Either y1 y2) r -> List Type)
ChoiceTy g1 g2 tl ctx = elimTri (b (fst (split tl))) (b' (snd (split tl))) (match ctx)

-- Choice operator
(+++) : {a, a' : _} -> Optic o => Context c o => ContextAdd c
     => {0 b : TypeList a -> c x1 s y1 r -> List Type}
     -> {0 b' : TypeList a' -> c x2 s y2 r -> List Type}
     -> (g1 : OpenGame o c a x1 s y1 r b) -> (g2 : OpenGame o c a' x2 s y2 r b')
     -> OpenGame o c
                 (a ++ a')
                 (Either x1 x2) s (Either y1 y2) r
                 (OpenGames.ChoiceTy g1 g2)
(+++) g h = MkGame
  (\tl => case split tl of (left, right) => g.play left ++++ h.play right)
  fn
    where
      fn : (tl : TypeList (a ++ a'))
        -> (body : c (Either x1 x2) s (Either y1 y2) r)
        -> TypeList (elimTri (b (fst (split {ys=a'} tl))) (b' (snd (split {xs=a} tl))) (match body))
      fn tl body with (split tl)
        fn tl body | (la, la') with (match body)
          fn tl body | (la, la') | (One x) = g.evaluate la x
          fn tl body | (la, la') | (Two x) = h.evaluate la' x
          fn tl body | (la, la') | (Both x y) = g.evaluate la x ++ h.evaluate la' y

Simultaneous : (Optic o, Context c o, ContextAdd c)
            => {a, a' : List Type}
            -> {b : TypeList a -> c x s y r -> List Type}
            -> {b' : TypeList a' -> c x' s' y' r' -> List Type}
            -> (g1 : OpenGame o c a x s y r b)
            -> (g2 : OpenGame o c a' x' s' y' r' b')
            -> TypeList (a ++ a') -> c (x, x') (s, s') (y, y') (r, r') -> List Type 
Simultaneous g1 g2 tl ctx = case (both (g1.play (fst (split tl))) (g2.play (snd (split tl))) ctx) of
                                 pair => b (fst (split tl)) (snd pair) ++ b' (snd (split tl)) (fst pair)

(&&&) : (Optic o, Context c o, ContextAdd c)
     => {a, a' : List Type}
     -> {0 b : TypeList a -> c x s y r -> List Type}
     -> {0 b' : TypeList a' -> c x' s' y' r' -> List Type}
     -> (g1 : OpenGame o c a x s y r b)
     -> (g2 : OpenGame o c a' x' s' y' r' b')
     -> OpenGame o c (a ++ a') (x, x') (s, s') (y, y') (r, r')
                     (Simultaneous g1 g2)
(&&&) g1 g2 = MkGame (\tl => let (tl1, tl2) = split tl in g1.play tl1 &&&& g2.play tl2)  
                     (fn g1 g2)
  where
    fn : (g1 : OpenGame o c a x s y r b)
      -> (g2 : OpenGame o c a' x' s' y' r' b')
      -> (ls : TypeList (a ++ a')) -> (v : c (x, x') (s, s') (y, y') (r, r')) 
      -> TypeList (Simultaneous g1 g2 ls v)
    fn g1 g2 ls v with (split ls)
      fn g1 g2 ls v | (ls1, ls2) with (both (g1.play ls1) (g2.play ls2) v)
        fn g1 g2 ls v | (ls1, ls2) | (c1, c2) = g1.evaluate ls1 c2 ++ g2.evaluate ls2 c1


ReplicateN : (n : Nat) -> List a -> List a
ReplicateN 0 xs = []
ReplicateN (S k) xs = xs ++ ReplicateN k xs

testRepl : ReplicateN 3 [Int, String] = [Int, String, Int, String, Int, String]
testRepl = Refl

-- population operator
-- population : (pop : Vect (S n) (OpenGame o c a x s y r b))
--           -> OpenGame o c (ReplicateN (S n) a) (Vect (S n) x) (Vect (S n) s) (Vect (S n) y) (Vect (S n) r) (\vs => ReplicateN (S n) ?nani)
-- 
mergeTwo : (Optic o, Context c o, ContextAdd c) => 
           {a : _} 
        -> {0 b : TypeList a -> c x s y r -> List Type}
        -> (g : OpenGame o c a x s y r b) 
        -> OpenGame o c (a ++ a) (x, x) (s, s) (y, y) (r, r) (Simultaneous g g)
mergeTwo g = g &&& g

0 Multipl : (Optic o, Context c o, ContextAdd c) => (k : Nat) -> (g : OpenGame o c a x s y r b) 
          -> (tl : TypeList a)
          -> o (Tuplelize (x :: replicate k x)) 
               (Tuplelize (s :: replicate k s)) 
               (Tuplelize (y :: replicate k y)) 
               (Tuplelize (r :: replicate k r)) 
Multipl 0 g tl = g.play tl
Multipl (S k) g tl = g.play tl &&&& Multipl k g tl

0
SimultaneousN : (Optic o, Context c o, ContextAdd c) => (n : Nat) -> (g : OpenGame o c a x s y r b) 
             -> TypeList (ReplicateN (S n) a) 
             -> c (Tuplelize (List.replicate (S n) x)) 
                  (Tuplelize (List.replicate (S n) s)) 
                  (Tuplelize (List.replicate (S n) y)) 
                  (Tuplelize (List.replicate (S n) r))
             -> List Type
SimultaneousN 0 g z w = b (rewrite sym $ appendNilRightNeutral a in z) w
SimultaneousN (S k) g z w = let rec = SimultaneousN k g (snd (split z)) (g.play (fst (split z)) // w) 
                             in b (fst (split z)) ((\\) {o} (Multipl k g (fst (split z))) w) ++ rec


mergeN :   (Optic o, Context c o, ContextAdd c) => (n : Nat)
        -> {a : _} 
        -> {0 b : TypeList a -> c x s y r -> List Type}
        -> (g : OpenGame o c a x s y r b) 
        -> OpenGame o c (ReplicateN (S n) a) 
                        (Tuplelize (List.replicate (S n) x))
                        (Tuplelize (List.replicate (S n) s))
                        (Tuplelize (List.replicate (S n) y))
                        (Tuplelize (List.replicate (S n) r))
                        (SimultaneousN n g)
mergeN n g = ?mergeN_rhs

{-
-}
