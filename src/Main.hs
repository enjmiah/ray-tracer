{-|
Interface for rendering images using the ray-tracer.
-}

{-# LANGUAGE TypeOperators #-}

module Main where

import Codec.Picture
import Data.Array (Array, listArray)
import Data.Array.Repa (U, D, Z (..), (:.)(..), (!))
import System.Random
import qualified Data.Array ((!))
import qualified Data.Array.Repa as R

import Trace


inf :: Floating a => a
inf = 1.0 / 0.0

nx :: Int
nx = 200 -- ^ Width of final image output
ny :: Int
ny = 100 -- ^ Height of final image output
ns :: Int
ns = 16 -- ^ Number of anti-aliasing samples to take per pixel
maxBounces :: Int
maxBounces = 8 -- ^ Maximum number of recursive bounces a ray is allowed before
               -- it is terminated


--- Define camera location and image plane
camera :: Camera
camera = makeCamera (3, 3, 2) (0, 0, -1) (0, 1, 0) 0.35
                    ((fromIntegral nx) / (fromIntegral ny)) 2

ground :: Primitive
ground  = makeSphere (0.0,-100.5,-1.0) 100.0 (makeDiffuse (0.8, 0.8, 0.0))
sphere1 :: Primitive
sphere1 = makeSphere (0.0, 0.0, -1.0) 0.5 (makeDiffuse (0.1, 0.2, 0.5))
sphere2 :: Primitive
sphere2 = makeSphere (1.0, 0.0, -1.0) 0.5 (makeMetallic (0.8, 0.6, 0.2) 0.3)
sphere3 :: Primitive
sphere3 = makeSphere (-1.0, 0.0, -1.0) 0.5 (makeRefractive 1.5)

-- | List of primitives making up world
world :: [Primitive]
world = [sphere1,sphere2,sphere3,ground]


data Camera = Camera { lowl :: Vec3
                     , horz :: Vec3
                     , vert :: Vec3
                     , orig :: Vec3
                     , lensRad :: Double
                     , basis :: (Vec3, Vec3, Vec3) } deriving (Show, Eq)


-- | Create a camera with the given parameters.
makeCamera :: Vec3 -> Vec3 -> Vec3 -> Double -> Double -> Double -> Camera
makeCamera position -- ^ Location of the camera in world space
           focus -- ^ Focal point
           up -- ^ Vector which points "up" from the perspective of the camera
           vfov -- ^ Vertical field of view in radians
           aspect -- ^ Aspect ratio (width / height)
           aperture -- ^ Diameter of the lens
           =
    let theta = vfov
        halfHeight = tan (theta / 2)
        halfWidth = aspect * halfHeight
        focusDist = norm (position - focus)
        -- u, v, w form a basis for camera space
        w = normalize (position - focus)
        u = normalize (up `cross` w)
        v = w `cross` u -- cross product of unit vectors is unit length
    in Camera { lowl = (position - halfWidth * focusDist .* u
                        - halfHeight * focusDist .* v - focusDist .* w)
              , horz = 2 * halfWidth * focusDist .* u
              , vert = 2 * halfHeight * focusDist .* v
              , orig = position
              , lensRad = aperture / 2
              , basis = (u, v, w) }


-- Get image with true color pixels from manifest Repa array.
toImage :: R.Array U R.DIM2 Vec3 -> Image PixelRGB16
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    s = round.(max 0).(min 0xffff)
    gen x y =
        let (red,green,blue) = a ! (Z :. x :. y)
        in PixelRGB16 (s red) (s green) (s blue)


--- get ray from camera to point u v on image plane
getRay :: Camera -> Double -> Double -> RNG -> (Ray, RNG)
getRay (Camera lowl horz vert orig lensRad (u, v, _)) s t rng =
    let ((randX, randY), newRng) = randomInUnitDisk rng
        offset = lensRad * randX .* u + lensRad * randY .* v
    in ((orig + offset, lowl + s .* horz + t .* vert - orig - offset), newRng)


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
getLineFixed :: IO String
getLineFixed =
    do
        line <- getLine
        return (fixDel line)


fixDel :: String -> String
fixDel st
    | '\DEL' `elem` st = fixDel (remDel st)
    | otherwise = st


remDel :: String -> String
remDel ('\DEL':t) = t
remDel (_:'\DEL':t) = t
remDel (_:t) = remDel t


--- function to generate REPA array of calculated pixel values
--- The D means its delayed, not actually calculated until we call
--- R.computeUnboxedP for parallel, or R.computeUnboxedS for serial
generateImgRepa :: R.Array D R.DIM2 Vec3
generateImgRepa = R.fromFunction (Z :. nx :. ny) (calcPixelAt camera world)


-- | Lazily generate an array of RNGs (one for each AA sample).
rngs :: Array Int RNG
rngs = listArray (0, count)
                 (take count [mkRNG seed | seed <- randoms (mkStdGen 0)])
    where count = ny * nx * ns + nx * ns + ns - 1


-- | Get the RNG for a given sample.
getRNG :: Int -> Int -> Int -> RNG
getRNG x y s = rngs Data.Array.! (y * nx * ns + x * ns + s)


--- function to calculate Pixel value at x y in a DIM2 Vec3 array/image plane
calcPixelAt :: Camera -> [Primitive] -> (Z :. Int :. Int) -> Vec3
calcPixelAt cam world (Z :. x :. y) =
    let (red, green, blue) = aaPixel cam world x (ny-y)
    in (65534.99*(sqrt red), 65534.99*(sqrt green), 65534.99*(sqrt blue))


-- | Return an anti-aliased pixel.
aaPixel :: Camera -> [Primitive] -> Int -> Int -> Vec3
aaPixel cam world x y =
    let sumColor = sum [randPixel cam world x y s | s <- [1..ns]]
    in (1 / fromIntegral ns) .* sumColor


--- helper for aaPixel generate a pixel vec with random pertubation to ray
randPixel :: Camera -> [Primitive] -> Int -> Int -> Int -> Vec3
randPixel cam world x y s =
    let (xrand:yrand:newRng) = getRNG x y s
        u   = ((fromIntegral x) + xrand) / (fromIntegral nx)
        v   = ((fromIntegral y) + yrand) / (fromIntegral ny)
        (ray, newRng2) = getRay cam u v newRng
    in colorPixel ray world maxBounces newRng2


-- | Return the sky color in the direction of an outgoing ray.
skyColor :: Ray -> Vec3
skyColor ray =
    let (_, y, _) = normalize (direction ray)
        blend = 0.5 * (y + 1.0) -- map [-1, 1] to [0, 1]
    in (1 - blend) .* (1.0, 1.0, 1.0) + blend .* (0.5, 0.7, 1.0)


--- takes ray from camera to point on image plane and produces color
colorPixel :: Ray -> [Primitive] -> Int -> RNG -> Vec3
colorPixel ray world maxBounces rng =
    case (hitInList ray 0.001 inf world) of
        Just record ->
            if maxBounces <= 0 then
                (0, 0, 0)
            else
                case (matHit record) ray record rng of
                    (Just (atten, scat), newRng) ->
                        atten * (colorPixel scat world (maxBounces - 1) newRng)
                    (Nothing, _) -> (0.0, 0.0, 0.0)
        Nothing -> skyColor ray


-- | Entry point for executable.
main :: IO ()
main = render
