module Data.Vect.Extra where

import Data.Vect.Type

--------------------------
--- Smart Constructors ---
--------------------------

mkVector2 :: (a, a) -> Vect ('S ('S 'Z)) a
mkVector2 (x, y) = x :. y :. Nil

mkVector3 :: (a, a, a) -> Vect ('S ('S ('S 'Z))) a
mkVector3 (x, y, z) = x :. y :. z :. Nil


---------------------
--- View Patterns ---
---------------------

viewTuple :: Vect ('S ('S 'Z)) a -> (a, a)
viewTuple (a :. b :. Nil) = (a, b)

viewList :: Vect n a -> [a]
viewList Nil = []
viewList (a :. as) = a : viewList as

------------------
--- Operations ---
------------------

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

vZipWith :: (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
vZipWith _ Nil Nil = Nil
vZipWith f (a :. as) (b :. bs) = (f a b) :. (vZipWith f as bs)

infixl 6 .+
(.+) :: Num a => Vect n a -> Vect n a -> Vect n a
(.+) = vZipWith (+)

scalarMult :: Num a => a -> Vect n a -> Vect n a
scalarMult a = fmap (* a)

infixl 7 .*
(.*) :: Num a => a -> Vect n a -> Vect n a
(.*) = scalarMult

vconcat :: Monoid a => Vect n a -> a
vconcat = fold

transform :: (Monoid (Vect m (Sum a)), Num a) => Matrix n m a -> Vect n a -> Vect m a
transform mat vect = getSum <$> (vconcat $ vZipWith ((<$$>) Sum . scalarMult) vect mat)

swapM :: Matrix ('S ('S 'Z)) ('S ('S 'Z)) Int
swapM = jhat :. ihat :. Nil

dilate :: Int -> Int -> Matrix ('S ('S 'Z)) ('S ('S 'Z)) Int
dilate x y = scalarMult x ihat :. scalarMult y jhat :. Nil

subV :: Num a => Vect n a -> Vect n a -> Vect n a
subV x y = x .+ (scalarMult (negate 1) y)

infixl 6 .-
(.-) :: Num a => Vect n a -> Vect n a -> Vect n a
(.-) = subV

----------------
--- Indexing ---
----------------

infixl 9 !!!
(!!!) :: Vect n a -> Int -> Maybe a
(!!!) Nil _       = Nothing
(!!!) (x :. _)    0 = Just x
(!!!) (_ :. rest) i = rest !!! (i - 1)


--------------------
--- Common Terms ---
--------------------

ihat :: Vect ('S ('S 'Z)) Int
ihat = 1 :. 0 :. Nil

jhat :: Vect ('S ('S 'Z)) Int
jhat = 0 :. 1 :. Nil

iden :: Matrix ('S ('S 'Z)) ('S ('S 'Z)) Int
iden = ihat :. jhat :. Nil


--------------------------------
--- Common Rotation Matrices ---
--------------------------------

ninetyCCW :: Num a => Matrix ('S ('S 'Z)) ('S ('S 'Z)) a
ninetyCCW = v1 :. v2 :. Nil
  where
    v1 :: Num a => Vect ('S ('S 'Z)) a
    v1 = 0 :. 1 :. Nil
    v2 :: Num a => Vect ('S ('S 'Z)) a
    v2 = negate 1 :. 0 :. Nil

oneeightyCCW :: Num a => Matrix ('S ('S 'Z)) ('S ('S 'Z)) a
oneeightyCCW = v1 :. v2 :. Nil
  where
    v1 :: Num a => Vect ('S ('S 'Z)) a
    v1 = negate 1 :. 0 :. Nil
    v2 :: Num a => Vect ('S ('S 'Z)) a
    v2 = 0 :. negate 1 :. Nil

twoseventyCCW :: Num a => Matrix ('S ('S 'Z)) ('S ('S 'Z)) a
twoseventyCCW = v1 :. v2 :. Nil
  where
    v1 :: Num a => Vect ('S ('S 'Z)) a
    v1 = 0 :. 1 :. Nil
    v2 :: Num a => Vect ('S ('S 'Z)) a
    v2 = negate 1 :. 0 :. Nil
