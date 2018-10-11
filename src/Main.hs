{-|
Interface for rendering images using the ray-tracer.
-}

{-# LANGUAGE TypeOperators #-}

module Main where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Array.Repa (fromListUnboxed, Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.Random
import qualified Codec.Picture.Types as M
import qualified Data.Array.Repa     as R -- for Repa

--- could move these Vec3, Ray, Hitable to there own module, then import
--- Vec3
type Vec3 = (Double,Double,Double)

instance (Num a, Num b, Num c) => Num(a, b, c) where
    (x1, y1, z1) + (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)
    (x1, y1, z1) - (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)
    (x1, y1, z1) * (x2, y2, z2) = (x1*x2, y1*y2, z1*z2)
    abs((x, y, z)) = (abs x, abs y, abs z)
    signum ((x, y, z)) = (signum x, signum y, signum z)
    fromInteger i = (fromInteger i, fromInteger i, fromInteger i)

instance (Fractional a, Fractional b, Fractional c) => Fractional (a, b, c) where
    (x1, y1, z1) / (x2, y2, z2) = (x1/x2, y1/y2, z1/z2)
    fromRational r = (fromRational r, fromRational r, fromRational r)


xcomp :: Vec3 -> Double
xcomp (el, _, _) = el
ycomp :: Vec3 -> Double
ycomp (_, el, _) = el
zcomp :: Vec3 -> Double
zcomp (_, _, el) = el

r :: Vec3 -> Double
r (el, _, _) = el
g :: Vec3 -> Double
g (_, el, _) = el
b :: Vec3 -> Double
b (_, _, el) = el

dot :: Vec3 -> Vec3 -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

veclength :: Vec3 -> Double
veclength v = sqrt (v `dot` v)

sqveclength :: Vec3 -> Double
sqveclength v = v `dot` v

cross :: Vec3 -> Vec3 -> Vec3
cross (x1, y1, z1) (x2, y2, z2) = ((y1*z2-z1*y2), (-x1*z2 - z1*x2), (x1*y2 - y1*x2))

unit_vector :: Vec3 -> Vec3
unit_vector v = v / (realToFrac (veclength v))

--- Ray, Condiser changing to data record, would involve some refactoring
type Ray = (Vec3,Vec3)

origin :: Ray -> Vec3
origin (o, _) = o

direction :: Ray -> Vec3
direction (_, d) = d

point_at :: Ray -> Double -> Vec3
point_at (o,d) t = o + d*(realToFrac t)

--- Hitable
data Hitable = Hitable {center::Vec3, radius::Double, material::Material} deriving (Show,Eq)
data Material = Diffuse Double Double Double | Metallic Double Double Double Double | Dielectric Double deriving (Show, Eq)


--- HitRecord
data HitRecord = HitRecord {t::Double, p::Vec3, normal::Vec3, matHit::Material} deriving (Show,Eq)


--- Calculate if ray hits hitable between tmin and tmax return Nothing on a miss
hit :: Ray -> Double -> Double -> Hitable -> Maybe HitRecord
hit (ro, rd) tmin tmax hitable =
    let sr   = radius hitable
        oc   = ro - (center hitable)
        a    = dot rd rd
        b    = dot oc rd
        c    = (dot oc oc) - (sr*sr)
        disc = (b*b) - (a*c)
    in if disc > 0 then
        let posroot = (-b -(sqrt disc))/a
            negroot = (-b + (sqrt disc))/a
        in if tmin < posroot && posroot < tmax then
            Just (genHitRecord posroot (ro,rd) hitable)
        else if tmin < negroot && negroot < tmax then
            Just (genHitRecord negroot (ro,rd) hitable)
        else
            Nothing
    else
        Nothing

--- helper for generating hitrecord
genHitRecord :: Double -> Ray -> Hitable -> HitRecord
genHitRecord t ray hitable =
    let p = point_at ray t
    in HitRecord t p ((p - (center hitable)) / (realToFrac (radius hitable))) (material hitable)

--- returns Just closest hit of ray to list of hitable objects or Nothing
hit_in_list :: Ray -> Double -> Double -> [Hitable] -> Maybe HitRecord
hit_in_list ray tmin tmax list = fst (foldl (hitacc ray tmin) (Nothing, tmax) list)

--- helper for hit in list
hitacc :: Ray -> Double -> (Maybe HitRecord, Double) -> Hitable -> (Maybe HitRecord, Double)
hitacc ray tmin (acc, closest) hitable =
    let maybehit = hit ray tmin closest hitable
    in case maybehit of
        Just rec -> (Just rec, t rec)
        Nothing -> (acc, closest)

-- Get image with true color pixels from manifest Repa array.
toImage :: Array U DIM2 Vec3 -> Image PixelRGB16
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    s = round.(max 0).(min 0xffff)
    gen x y =
        let (red,green,blue) = a ! (Z :. x :. y)
        in PixelRGB16 (s red) (s green) (s blue)

--- Camera
data Camera = Camera {lowl::Vec3,horz::Vec3,vert::Vec3,orig::Vec3} deriving (Show,Eq)

--- get ray from camera to point u v on image plane
get_ray:: Camera -> Double -> Double -> Ray
get_ray cam u v = ((orig cam), (lowl cam)+ ((realToFrac u)*(horz cam))+((realToFrac v)*(vert cam))-(orig cam))

--- ask for path to save rendered image on then compute the image
render :: IO ()
render = do
  putStrLn "Path to save image in?"
  path <- getLineFixed
  --- Computes each pixel in //, stores in REPA array of type Array U DIM2 Vec3
  img <- R.computeUnboxedP generateImgRepa
  --- Converts to Juicy Pixel ImageRGB16 and saves it to disk as a png
  (savePngImage path . ImageRGB16 . toImage) img

--- from A3 sol to clean up user input
getLineFixed =
    do
        line <- getLine
        return (fixDel line)

fixDel st
    | '\DEL' `elem` st = fixDel (remDel st)
    | otherwise = st

remDel ('\DEL':t) = t
remDel (h:'\DEL':t) = t
remDel (h:t) = remDel t

--- Width and Height of Image
nx = 200::Int
ny = 100::Int
ns = 100::Int
maxdepth = 50 :: Int

--- Define camera location and image plane
camera = Camera (-2.0, -1.0, -1.0) (4.0, 0.0, 0.0) (0.0, 2.0, 0.0) (0.0, 0.0, 0.0)
--- List of hitable object making up world

sphere1 = Hitable (0.0, 0.0, -1.0) 0.5 (Diffuse 0.1 0.2 0.5)
ground  = Hitable (0.0,-100.5,-1.0) 100.0 (Diffuse 0.8 0.8 0.0)
sphere2 = Hitable (1.0, 0.0, -1.0) 0.5 (Metallic 0.8 0.6 0.2 0.3)
sphere3 = Hitable (-1.0, 0.0, -1.0) 0.5 (Dielectric 1.5)

world   = [sphere1,sphere2,sphere3,ground]

--- function to generate REPA array of calculated pixel values
--- The D means its delayed, not actually calculated until we call
--- R.computeUnboxedP for parallel, or R.computeUnboxedS for serial
generateImgRepa :: Array D DIM2 Vec3
generateImgRepa = R.fromFunction (Z :. nx :. ny) (calcPixelAt camera world)

--- function to calculate Pixel value at x y in a DIM2 Vec3 array/image plane
calcPixelAt :: Camera -> [Hitable] -> (Z :. Int :. Int) -> Vec3
calcPixelAt cam world (Z :. x :. y) =
    let (red, green, blue) = aaPixel cam world x (ny-y)
    in (65534.99*(sqrt red), 65534.99*(sqrt green), 65534.99*(sqrt blue))

--- randomize pixel colors for aa
--- ran into issues trying to make this parallel so resorted to list comp
aaPixel :: Camera -> [Hitable] -> Int -> Int -> Vec3
aaPixel cam world x y =
    let sumColor = sum [randPixel cam world x y s | s <- [1..ns]]
    in sumColor/(fromIntegral ns)

--- helper for aaPixel generate a pixel vec with random pertubation to ray
randPixel :: Camera->[Hitable]-> Int -> Int -> Int -> Vec3
randPixel cam world x y s =
    let (xrand:yrand:tl)  = take 2 $ randoms (mkStdGen s) :: [Double]
        u      = ((fromIntegral x) + xrand) / (fromIntegral nx)
        v      = ((fromIntegral y) + yrand) / (fromIntegral ny)
        ray    = get_ray cam u v
    in colorPixel ray world s 0

--- takes ray from camera to point on image plane and produces color
colorPixel :: Ray -> [Hitable] -> Int -> Int -> Vec3
colorPixel ray world seed depth =
    let maybehit  = hit_in_list ray 0.001 (1.0/0.0) world
    in case maybehit of
        Just record ->
            if depth >= maxdepth then
                (0.0,0.0,0.0)
            else
                case (scatter ray record seed) of
                    Just (atten,scat) -> atten * (colorPixel scat world (seed+1) (depth+1))
                    Nothing -> (0.0,0.0,0.0)
        Nothing ->
            let ud = unit_vector (direction ray)
                tt = 0.5*(g ud)+1.0
            in (realToFrac (1.0-tt))*(1.0,1.0,1.0) + (realToFrac tt)*(0.5,0.7,1.0)

--- this sucks for getting random unit spheres
random_in_unit_sphere :: Int -> Vec3
random_in_unit_sphere seed =
    let (x:y:z:tl) = take 3 $ randoms (mkStdGen seed) :: [Double]
        vec = (x,y,z)
        len = sqveclength vec
    in if len >= 1 then random_in_unit_sphere (seed +1) else vec

scatter :: Ray -> HitRecord -> Int -> Maybe (Vec3,Ray)
scatter ray record seed =
    let mat = matHit record
        pp = p record
        n = normal record
    in case mat of
        Diffuse x y z ->
            let target = pp + n + random_in_unit_sphere seed
                alb    = (x,y,z)
                scat   = (pp,target-pp)
            in Just (alb,scat)
        Metallic x y z f ->
            let refl = reflect (unit_vector (direction ray)) n
                alb  = (x,y,z)
                fuzz = max 0.0 (min 1.0 f)
                scat = (pp, refl + (random_in_unit_sphere seed)*(realToFrac fuzz))
            in if (refl `dot` n) > 0 then
                Just (alb,scat)
            else
                Nothing
        Dielectric ref_idx ->
            let d = direction ray
                refl = reflect d n
                ddn = d `dot` n
                (out,ni_over_nt,cosine) = if ddn > 0 then
                        let out = -n
                            ni_over_nt = ref_idx
                            cosine = ref_idx * ddn / (veclength d)
                        in (out,ni_over_nt,cosine)
                    else
                        let out = n
                            ni_over_nt = 1.0 / ref_idx
                            cosine = -ddn/ (veclength d)
                        in (out,ni_over_nt,cosine)
                maybeRefracted = refract d out ni_over_nt
                refl_prob = case maybeRefracted of
                    Just refrected -> schlick cosine ref_idx
                    Nothing -> 1.0
            in if (fst (random (mkStdGen seed)::(Double, StdGen))) < refl_prob then
                Just ((1.0,1.0,1.0),(pp,refl))
            else
                case maybeRefracted of
                    Just refracted -> Just ((1.0,1.0,1.0),(pp,refracted))
                    Nothing -> Nothing

reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (n*(realToFrac (v `dot` n))*(realToFrac 2.0))

refract :: Vec3 -> Vec3 -> Double -> Maybe Vec3
refract v n ni_over_nt =
    let uv   = unit_vector v
        dt   = uv `dot` n
        disc = 1.0 - ni_over_nt*ni_over_nt*(1.0-dt*dt)
    in if disc > 0 then
        Just ((uv - n*(realToFrac dt))*(realToFrac ni_over_nt) - n*(realToFrac (sqrt disc)))
    else
        Nothing

schlick :: Double -> Double -> Double
schlick cosine ref_idx =
    let r0 = (1-ref_idx) / (1+ref_idx)
        rr = r0*r0
    in  (rr - (1-rr)*((1.0-cosine)^^5))

main :: IO ()
main = render
