{-# LANGUAGE ExistentialQuantification #-}

import System.IO
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Immutable.Shuffle

data Point c = Real c => P { x, y :: c }

data Span c = Real c => S (Maybe c) (Maybe c)

data Range c = R (Span c) (Span c)

data Axis = Ox | Oy deriving (Eq, Bounded, Enum)

data Btree a b = Leaf b | Node a (Btree a b) (Btree a b)

type KDtree c = Btree c (Vector (Point c))

nextAxis a = if a /= maxBound then succ a else minBound

coordOf ax = case ax of Ox -> x; Oy -> y

leftOrSame a (P x1 y1) (P x2 y2) | a == Ox = x2 < x1 || x2 == x1 && y2 <= y1
                                 | a == Oy = y2 < y1 || y2 == y1 && x2 <= x1


dist2p (P x1 y1) (P x2 y2) = sqrt ((realToFrac x1 - realToFrac x2)**2 + (realToFrac y1 - realToFrac y2)**2)


isLT a b = realToFrac a < realToFrac b


zero d = abs d < 0.0001


type Coord = Int


readListOfLists = map (map (read :: String -> Coord) . words) . lines


pointVector as = V.fromList $ map (\[x,y] -> P x y) as

-- pointVector :: ... -> ...


median5 pts ax = med (pts ! 0) (pts ! (div h 2)) (pts ! h) (pts ! (div (n+h) 2)) (pts ! n) where
  med a b c d e | a >> b = med b a c d e
                | b >> c = med a c b d e
                | c >> d = med a b d c e
                | d >> e = med a b c e d
                | otherwise   = c
  p >> q = not (leftOrSame ax q p)
  n = V.length pts - 1
  h = div n 2


buildKDT pts mqty = f pts Ox where
  f pts ax =
    if V.length pts <= mqty then Leaf pts
    else let m = median5 pts ax
             split = coordOf ax m
             (pts1, pts2) = V.unstablePartition (leftOrSame ax m) pts
             nxta = nextAxis ax
         in Node split (f pts1 nxta) (f pts2 nxta)


nearestPoint p kdt = f p kdt Ox where
  f p (Leaf pts) _ = V.foldl g Nothing pts where
    g mp p' = if zero d || isJust mp && snd (fromJust mp) <= d then mp else Just (p',d)
      where d = dist2p p p'
  f p (Node split t1 t2) ax =
    case if diff >= 0 then
           let p1 = f p t1 nxta
               p2 = if isNothing p1 || isLT diff (snd (fromJust p1)) then f p t2 nxta else Nothing
           in (p1,p2)
         else
           let p2 = f p t2 nxta
               p1 = if isNothing p2 || isLT (-diff) (snd (fromJust p2)) then f p t1 nxta else Nothing
           in (p1,p2)
      of (p, Nothing) -> p; (Nothing, p) -> p
         (p1@(Just (_,d1)), p2@(Just (_,d2))) -> if (d1 <= d2) then p1 else p2
    where pos = coordOf ax p
          diff = split-pos
          nxta = nextAxis ax


pointsInRange srng kdt = f kdt (R (S Nothing Nothing) (S Nothing Nothing)) Ox where
  R (S (Just sx1) (Just sx2)) (S (Just sy1) (Just sy2)) = srng
  f (Leaf pts) _ _ = V.filter withinRange pts where
    withinRange p = let P xp yp = p in sx1 <= xp && xp <= sx2 && sy1 <= yp && yp <= sy2
  f (Node split t1 t2) rng ax =
    let R (xspan@(S x1 x2)) (yspan@(S y1 y2)) = rng
    in if isJust x1 && sx1 <= fromJust x1 && isJust x2 && fromJust x2 <= sx2 &&         -- целият възел е вътре
          isJust y1 && sy1 <= fromJust y1 && isJust y2 && fromJust y2 <= sy2
       then let ptsFrom (Leaf pts) = pts
                ptsFrom (Node _ t1 t2) = ptsFrom t1 V.++ ptsFrom t2
            in ptsFrom t1 V.++ ptsFrom t2
       else if isJust x1 && sx2 < fromJust x1 || isJust x2 && fromJust x2 < sx1 ||      -- целият възел е вън
               isJust y1 && sy2 < fromJust y1 || isJust y2 && fromJust y2 < sy1
            then V.empty
            else let nxta = nextAxis ax                                                 -- търсене в поддърветата
                     js = Just split
                     (rng1, rng2) = case ax of Ox -> (R (S x1 js) yspan, R (S js x2) yspan)
                                               Oy -> (R xspan (S y1 js), R xspan (S js y2))
                 in f t1 rng1 nxta V.++ f t2 rng2 nxta


{- Четене от стандартния вход на редицата от координати (по една точка на ред), разбъркване на редицата,
   построяване на k-d дърво от точките, извършване на действия с дървото, извеждане на резултат. -}
main = do hSetBuffering stdin LineBuffering
          hSetBuffering stdout LineBuffering
          contents <- getContents
          v <- shuffleM $ pointVector $ readListOfLists contents
          print $ process v


process pts = let kdt = buildKDT pts 10 in
             -- kdt                          
                nearestPoint (P 12 13) kdt   
             -- pointsInRange (R (S (Just 0) (Just 100)) (S (Just 20) (Just 80))) kdt    


instance Show c => Show (Point c) where
  show (P x y) = "P|" ++ show x ++ "," ++ show y ++ "|"

instance Show c => Show (Span c) where
  show (S s1 s2) = "S|" ++ show s1 ++ "," ++ show s2 ++ "|"

-- Образуване на низ – образ на двоично дърво с отстъпи за поддърветата и обозначаване на леви и десни поддървета.
instance (Show a, Show b) => Show (Btree a b) where
  show t = f t 0 "" where
    f (Leaf v) n p = offset n p ++ show v ++ "\n"
    f (Node v t1 t2) n p = offset n p ++ show v ++ "\n" ++ f t1 (n+1) "L: " ++ f t2 (n+1) "R: "
    offset n p = (concat $ replicate n "    ") ++ p
