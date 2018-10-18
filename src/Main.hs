{-|
Interface for rendering images using the ray-tracer.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Codec.Picture
import Data.Array.Repa (Z (..), (:.)(..))
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Read
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Array.Repa as R

import Scene (world, skyColor1, skyColor2)
import Trace


-- | Infinity
inf :: Floating a => a
inf = 1.0 / 0.0


-- | Render options
data Options = Options {
    -- | Number of anti-aliasing samples to take per pixel
    samples :: Int,
    -- | Maximum number of recursive bounces a ray is allowed before it is
    -- terminated
    maxBounces :: Int,
    -- | Output file
    output :: String,
    -- | Gamma correction
    gamma :: Double
} deriving (Show, Eq)

-- | Options to use when not overridden by the user
defaultOptions :: Options
defaultOptions = Options {
    samples=16, maxBounces=8, output="out.png", gamma = 2.2
}


-- | Parse an option string into a positive integer.  Raises an error if such an
-- operation is not possible.
parsePositiveInt :: String -> String -> Int
parsePositiveInt str optName =
    case (readMaybe str :: Maybe Int) of
        Just x -> if x > 0 then x else error errorMsg
        Nothing -> error errorMsg
    where errorMsg = optName ++ " option must be a positive integer"

parsePositiveDouble :: String -> String -> Double
parsePositiveDouble str optName =
    case (readMaybe str :: Maybe Double) of
        Just x -> if x > 0.0 then x else error errorMsg
        Nothing -> error errorMsg
    where errorMsg = optName ++ " option must be a positive Double"    


-- | Available command-line options for our program.
options :: [OptDescr (Options -> Options)]
options = [
    Option "s" ["samples"]
           (ReqArg (\ ns opts -> opts { samples = parsePositiveInt ns "samples" })
                   "SAMPLES")
           "Number of anti-aliasing samples to take per pixel",
    Option "b" ["bounces"]
           (ReqArg (\ b opts -> opts { maxBounces = parsePositiveInt b "bounces" })
                   "BOUNCES")
           "Maximum number of recursive bounces a ray is allowed before it is terminated",
    Option "o" ["output"]
           (ReqArg (\ f opts -> opts { output = f }) "FILE")
           "Output file (should end in \"png\")",
    Option "g" ["gamma"]
            (ReqArg (\ g opts -> opts { gamma = parsePositiveDouble g "gamma" })
                   "BOUNCES")
           "Gamma correction applied to rendered image"
    ]


-- | Get image with true color pixels from manifest Repa array.
toImage :: R.Array R.U R.DIM2 Vec3 -> Image PixelRGB16
toImage a = generateImage gen width height
    where Z :. width :. height = R.extent a
          s = round . (max 0) . (min 0xffff)
          gen x y =
              let (red,green,blue) = a R.! (Z :. x :. y)
              in PixelRGB16 (s red) (s green) (s blue)


-- | Get ray from camera to point u v on image plane
getRay :: Camera -> Double -> Double -> RNG -> (Ray, RNG)
getRay (Camera lowl horz vert orig lensRad _ (u, v, _) ) s t rng =
    let ((randX, randY), newRng) = randomInUnitDisk rng
        offset = lensRad * randX .* u + lensRad * randY .* v
    in ((orig + offset, lowl + s .* horz + t .* vert - orig - offset), newRng)


-- | Render an image with the provided options (and scene)
render :: Options -> (Camera, [Primitive]) -> IO ()
render options world = do
    -- compute each pixel in //, stores in REPA array
    img <- R.computeUnboxedP (generateImgRepa world options)
    -- convert to Juicy Pixel ImageRGB16 and save it to disk as a PNG
    (savePngImage (output options) . ImageRGB16 . toImage) img


-- | Generate a REPA array of calculated pixel values.
-- The 'R.D' means the calculation is delayed until we call 'R.computeUnboxedP'
-- for parallel evaluation, or 'R.computeUnboxedS' for serial.
generateImgRepa :: (Camera, [Primitive]) -> Options -> R.Array R.D R.DIM2 Vec3
generateImgRepa !world !options =
    let (!camera, !primitives) = world
        (!nx, !ny) = size camera
    in R.fromFunction (Z :. nx :. ny) (calcPixelAt camera primitives options)


-- | Calculate the pixel value at x y in an image plane.
calcPixelAt :: Camera -> [Primitive] -> Options -> (Z :. Int :. Int) -> Vec3
calcPixelAt cam world options (Z :. x :. y) =
    let (_, ny) = size cam
        (red, green, blue) = aaSample cam world x (ny - y) options
    -- convert to an "integer" (will get rounded properly later)
    in (65534.99*(red ** (1.0 / (gamma options))), 65534.99*(green ** (1.0/(gamma options))), 65534.99*(blue ** (1.0/(gamma options))) )


-- | Given a range of integer inputs, return integer outputs that appear to be
-- uncorrelated to the supplied inputs.  We use this function to generate
-- "random" seeds.
decorrelate :: Int -> Int
decorrelate x =
    -- very high-frequency sine works well here for producing "random-ness"
    unsafeCoerce (sin (fromIntegral (20000000000 * x) :: Double))


-- | Return an anti-aliased sample in linear colorspace.  The anti-aliased
-- sample is calculated as the average of several random samples.
aaSample :: Camera -> [Primitive] -> Int -> Int -> Options -> Vec3
aaSample cam world x y options =
    let ns = samples options
        (nx, _) = size cam
        sumColor =
            sum [randSample cam world x y options
                            (mkRNG (decorrelate (y*nx*ns + x*ns + s)))
                            | s <- [1..ns]]
    in (1 / fromIntegral ns) .* sumColor


-- | Return a sample in linear colorspace.  The position of the sample will be
-- randomly jittered slightly based on the RNG.
randSample :: Camera -> [Primitive] -> Int -> Int -> Options -> RNG -> Vec3
randSample cam world x y options rng =
    let (nx, ny) = size cam
        (xrand:yrand:newRng) = rng
        u   = ((fromIntegral x) + xrand) / (fromIntegral nx)
        v   = ((fromIntegral y) + yrand) / (fromIntegral ny)
        (ray, newRng2) = getRay cam u v newRng
    in intersect ray world (maxBounces options) newRng2


-- | Return the sky color in the direction of an outgoing ray.
skyColor :: Ray -> Vec3
skyColor ray =
    let (_, y, _) = normalize (direction ray)
        blend = 0.5 * (y + 1.0) -- map [-1, 1] to [0, 1]
    in (1 - blend) .* skyColor2 + blend .* skyColor1


-- | Traces a ray through a scene and produces color
intersect :: Ray -> [Primitive] -> Int -> RNG -> Vec3
intersect ray world maxBounces rng =
    case (hitInList ray 0.001 inf world) of
        Just record ->
            if maxBounces <= 0 then
                (0, 0, 0)
            else
                case (matHit record) ray record rng of
                    (Just (atten, scat), newRng) ->
                        atten * (intersect scat world (maxBounces - 1) newRng)
                    (Nothing, _) -> (0.0, 0.0, 0.0)
        Nothing -> skyColor ray


-- | Entry point for executable.
-- Follows structure of example in
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/System-Console-GetOpt.html#t:ArgDescr
main :: IO ()
main = do
    let header = "Usage: ray-tracer [OPTION...]"
    argv <- getArgs
    case getOpt Permute options argv of
        (opts, _, []) ->
            do let options = (foldl (flip id) defaultOptions opts)
               putStrLn (show options)
               render options world
        (_, _, errs) ->
            ioError (userError (concat errs ++ usageInfo header options))
