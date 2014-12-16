import Control.Monad
import Data.List
import Data.Bits
import Data.Int (Int64)
import Text.Printf

-- q = 0,1,2,3

centerCoord :: Int -> Int
centerCoord x
  | abs (x-1) <= 1 = 1
  | abs (x-4) <= 1 = 4
  | otherwise      = error "bad coordinate"

center :: Int -> (Int,Int)
center q
  | q == 0 = (1,1)
  | q == 1 = (1,4)
  | q == 2 = (4,1)
  | q == 3 = (4,4)
  | otherwise = error "bad quadrant"

rotate_ cw q (x,y) 
  | (cx,cy) /= (qx,qy) = (x,y)
  | otherwise          = (cx - s*dy, cy + s*dx)
  where
    (qx,qy) = center q
    cx = centerCoord x
    cy = centerCoord y
    dx = x-cx
    dy = y-cy
    s = if cw then 1 else -1

rotateCW q (x,y) = rotate_ True q (x,y)
rotateCCW q (x,y) = rotate_ False q (x,y) 

bitIndex (x,y) = x*6+y

showMatrix m m' =  unlines [  unwords (compareRow r r') | (r,r') <-  zip m m' ]

compareRow xs ys = zipWith go xs ys
  where go x y = if x == y then "  .  " else show y

showRotation q cw = do
  let m = [ [ (i,j) | j <- [0..5] ] | i <- [0..5] ]
      m' = map (map (rotate_ cw q) ) m
      c = if cw then "clockwise" else "counter-clockwise"
  putStrLn $ "Rotation of quadrant " ++ show q ++ " " ++ c 
  putStrLn $ showMatrix m m' 

test1 = showRotation 0 True

pairs xs = zip xs (tail $ cycle xs)

genRotation q cw = do
  let (qx,qy) = center q
      xy1 = (qx-1,qy-1)
      xy2 = (qx-1,qy)
      chain1 = take 4 $ iterate (rotate_ (not cw) q) xy1
      chain2 = take 4 $ iterate (rotate_ (not cw) q) xy2
  -- putStrLn $ "cw chain1: " ++ intercalate " - " (map show chain1)
  -- putStrLn $ "cw chain2: " ++ intercalate " - " (map show chain2)
  let mask1 = foldl' setBit (0::Int64) indx1
      mask2 = foldl' setBit (0::Int64) indx2
      indx1 = map bitIndex chain1
      indx2 = map bitIndex chain2
  -- putStrLn $ printf "mask1 = 0x%010x" mask1
  -- putStrLn $ printf "mask2 = 0x%010x" mask2
  let trans1 =  intercalate " . " [ "tb v " ++ show i ++ " " ++ show j | (i,j) <- pairs indx1 ]
      trans2 =  intercalate " . " [ "tb v " ++ show i ++ " " ++ show j | (i,j) <- pairs indx2 ]
  -- putStrLn $ "trans1: " ++ trans1
  -- putStrLn $ "trans2: " ++ trans2
  let code = [ name ++ " v = maskbits (maskbits v m1 b1) m2 b2"
             , "  where"
             , "    m1 = " ++ printf "0x%010x" mask1
             , "    b1 = " ++ trans1 ++ " $ 0"
             , "    m2 = " ++ printf "0x%010x" mask2
             , "    b2 = " ++ trans2 ++ " $ 0"
             ]
      name = "rot" ++ show q ++ (if cw then "cw" else "ccw")
  putStrLn $ unlines code

{-# INLINE maskbits #-}
maskbits v m b = (v .&. (complement m) .|. b)

{-# INLINE tb #-}
-- tb v i j m = if testBit i v then setBit j m else m
tb v i j m =  m .|. (rotate  (v .&. (bit i)) (j-i))

genRotations = do
  let defs = [ "{-# INLINE maskbits #-}"
             , "maskbits v m b = (v .&. (complement m) .|. b)"
             , ""
             , "{-# INLINE tb #-}"
             , "tb v i j m =  m .|. (rotate  (v .&. (bit i)) (j-i))"
             ]

  putStrLn $ unlines defs

  forM_ [0..3] $ \q ->
    forM_ [True,False] $ \cw -> do
      genRotation q cw
      putStrLn ""

main = genRotations


