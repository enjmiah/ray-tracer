{-|
Module for performing path tracing.
-}

module Trace where

import Data.Int (Int64)
import System.Random
import Unsafe.Coerce (unsafeCoerce)


----------
-- Math --
----------

-- | type for most calculations in ray tracer 
type Vec3 = (Double,Double,Double)

-- | Provide +, - for 'Vec3's
instance (Num a, Num b, Num c) => Num (a, b, c) where
    (x1, y1, z1) + (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)
    (x1, y1, z1) - (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)
    (x1, y1, z1) * (x2, y2, z2) = (x1*x2, y1*y2, z1*z2)
    abs((x, y, z)) = (abs x, abs y, abs z)
    signum ((x, y, z)) = (signum x, signum y, signum z)
    fromInteger i = (fromInteger i, fromInteger i, fromInteger i)

-- | Scalar vector multiplication.
(.*) :: Double -> Vec3 -> Vec3
infixl 7 .* -- same as regular *
s .* (x, y, z) = (s * x, s * y, s * z)

-- | Compute the dot product of two 'Vec3's.
dot :: Vec3 -> Vec3 -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

-- | Return the Euclidean norm of a 'Vec3'.
norm :: Vec3 -> Double
norm v = sqrt (v `dot` v)

-- | Return the squared Euclidean norm of a 'Vec3'.
squaredNorm :: Vec3 -> Double
squaredNorm v = v `dot` v

-- | Return the cross product of two vectors
cross :: Vec3 -> Vec3 -> Vec3
cross (x1, y1, z1) (x2, y2, z2) =
    ((y1*z2 - z1*y2), -(x1*z2 - z1*x2), (x1*y2 - y1*x2))

-- | Return the input vector normalized to unit length
normalize :: Vec3 -> Vec3
normalize v = (1 / norm v) .* v

-- | Standard mathematical ray consisting of '(origin, direction)'.
type Ray = (Vec3, Vec3)

-- | Return the origin of a vector.
origin :: Ray -> Vec3
origin (o, _) = o

-- | Return the direction of a vector.
direction :: Ray -> Vec3
direction (_, d) = d

-- | Compute the point at the given parameter position along a ray.
pointAt :: Ray -> Double -> Vec3
pointAt (o, d) t = o + t .* d


-------------
-- Shading --
-------------

-- | Calculate if ray hits primitive between tmin and tmax return 'Nothing' on a
-- miss
type Primitive = Ray -> Double -> Double -> Maybe HitRecord

-- | Takes an incoming ray and and its associated 'HitRecord', and returns the
-- attenuated color and an outgoing scatter ray, or 'Nothing' for a miss.
type Material = Ray -> HitRecord -> RNG -> (Maybe (Vec3, Ray), RNG)

-- | Records information about a ray intersection.
data HitRecord = HitRecord { t :: Double
                           , point :: Vec3
                           , normal :: Vec3
                           , matHit :: Material }


-- | Create a sphere primitive from a center, a radius, and a 'Material'.
makeSphere :: Vec3 -> Double -> Material -> Primitive
makeSphere center radius material = intersector
    where intersector :: Primitive
          intersector (origin, direction) tMin tMax =
              let oc   = origin - center
                  -- a, b, c, and discriminant as in the quadratic formula
                  a = squaredNorm direction
                  b = dot oc direction
                  c = (dot oc oc) - (radius * radius)
                  discriminant = (b * b) - (a * c)
              in if discriminant > 0 then
                  let sqrtDisc = sqrt discriminant
                      root1 = (-b - sqrtDisc) / a
                      root2 = (-b + sqrtDisc) / a
                  in if tMin < root1 && root1 < tMax then
                      Just (genHitRecord root1 (origin, direction))
                  else if tMin < root2 && root2 < tMax then
                      Just (genHitRecord root2 (origin, direction))
                  else
                      Nothing
              else
                  Nothing
          genHitRecord :: Double -> Ray -> HitRecord
          genHitRecord t ray =
              let p = pointAt ray t
                  normal = (1 / radius) .* (p - center)
              in HitRecord t p normal material


-- | Returns Just closest hit of ray to list of 'Primitive's or 'Nothing'
hitInList :: Ray -> Double -> Double -> [Primitive] -> Maybe HitRecord
hitInList ray tmin tmax list =
    fst (foldl (hitacc ray tmin) (Nothing, tmax) list)
    where hitacc :: Ray -> Double -> (Maybe HitRecord, Double) -> Primitive
                        -> (Maybe HitRecord, Double)
          hitacc ray tmin (acc, closest) primitive =
              let maybehit = primitive ray tmin closest
              in case maybehit of
                  Just rec -> (Just rec, t rec)
                  Nothing -> (acc, closest)


-- | Make an ideal Lambertian diffuse material with the given albedo colour.
makeDiffuse :: Vec3 -> Material
makeDiffuse albedo = scatter
    where scatter :: Material
          -- note that Lambertian diffuse materials don't care about incoming ray
          scatter _ record rng =
              let pp = point record
                  n = normal record
                  (randvec, newRng) = randomInUnitSphere rng
                  target = pp + n + randvec
                  scattered = (pp, target - pp)
              in (Just (albedo, scattered), newRng)


-- | Make a metallic material with given albedo and roughness.  Roughness must
-- be in the [0, 1] range.
makeMetallic :: Vec3 -> Double -> Material
makeMetallic albedo roughness = scatter
    where scatter :: Material
          scatter ray record rng =
              let pp = point record
                  n = normal record
                  refl = reflect (normalize (direction ray)) n
                  fuzz = max 0.0 (min 1.0 roughness)
                  (randvec, newRng) = randomInUnitSphere rng
                  scattered = (pp, refl + fuzz .* randvec)
              in if (refl `dot` n) > 0 then
                  (Just (albedo, scattered), newRng)
              else
                  (Nothing, newRng)


-- | Make a refractive material with the given refractive index.
makeRefractive :: Double -> Material
makeRefractive refIdx = scatter
    where scatter :: Material
          scatter ray record rng =
              let pp = point record
                  n = normal record
                  d = direction ray
                  refl = reflect d n
                  ddn = d `dot` n
                  (out,etaIOverEtaT,cosine) =
                      if ddn > 0 then
                          let out = -n
                              etaIOverEtaT = refIdx
                              cosine = refIdx * ddn / (norm d)
                          in (out,etaIOverEtaT,cosine)
                      else
                          let out = n
                              etaIOverEtaT = 1.0 / refIdx
                              cosine = -ddn/ (norm d)
                          in (out,etaIOverEtaT,cosine)
                  maybeRefracted = refract d out etaIOverEtaT
                  reflProb =
                      case maybeRefracted of
                          Just _ -> schlick cosine refIdx
                          Nothing -> 1.0
                  (rand:newRng) = rng
              in if rand < reflProb then
                  (Just ((1.0, 1.0, 1.0), (pp, refl)), newRng)
              else
                  case maybeRefracted of
                      Just refracted -> (Just ((1.0,1.0,1.0),(pp,refracted)), newRng)
                      Nothing -> (Nothing, newRng)


-- | Reflect the vector 'v' across 'n'.
reflect :: Vec3 -> Vec3 -> Vec3
reflect v n = v - (2 * (v `dot` n)) .* n


-- | Refract a vector through a surface with normal 'n'.  If refraction is
-- impossible due to Snell's law not having a solution (this usually means total
-- internal reflection), return 'Nothing'.
refract :: Vec3 -> Vec3 -> Double -> Maybe Vec3
refract v n etaIOverEtaT =
    let uv   = normalize v
        dt   = uv `dot` n
        disc = 1.0 - etaIOverEtaT*etaIOverEtaT*(1.0-dt*dt)
    in if disc > 0 then
        Just (etaIOverEtaT .* (uv - dt .* n) - (sqrt disc) .* n)
    else
        Nothing


-- | Compute Shlick's approximation to the Fresnel factor based on the given
-- refractive index.
schlick :: Double -> Double -> Double
schlick cosine refIdx =
    let r0 = (1-refIdx) / (1+refIdx)
        rr = r0*r0
    in  (rr - (1 - rr) * ((1 - cosine) ** 5))


------------
-- Camera --
------------
-- | Record type for camera location, orientation, view plane.
data Camera = Camera { lowl :: Vec3
                     , horz :: Vec3
                     , vert :: Vec3
                     , orig :: Vec3
                     , lensRad :: Double
                     , size :: (Int, Int)
                     , basis :: (Vec3, Vec3, Vec3) } deriving (Show, Eq)


-- | Create a camera with the given parameters.
makeCamera :: Vec3 -> Vec3 -> Vec3 -> Double -> (Int, Int) -> Double -> Camera
makeCamera position -- ^ Location of the camera in world space
           focus -- ^ Focal point
           up -- ^ Vector which points "up" from the perspective of the camera
           vfov -- ^ Vertical field of view in radians
           (width, height) -- ^ Size in pixels of final output image
           aperture -- ^ Diameter of the lens
           =
    let theta = vfov
        halfHeight = tan (theta / 2)
        aspect = ((fromIntegral width) / (fromIntegral height)) :: Double
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
              , size = (width, height)
              , lensRad = aperture / 2
              , basis = (u, v, w) }


------------
-- Random --
------------

-- | An infinite list of random 'Double's in the range [0.0, 1.0).
-- The list being infinite is not checked!
type RNG = [Double]


-- | Create an infinite list of random 'Double's in the range [0.0, 1.0).
-- The interval being half-open is important, since it can avoid division by
-- zero problems.
mkRNG :: Int -> RNG
mkRNG seed = randomRs (0.0, prevToOne) (mkStdGen seed)
    where prevToOne :: Double -- largest Double < 1.0
          prevToOne = unsafeCoerce (((unsafeCoerce (1 :: Double)) :: Int64) - 1)


-- | Return a random vector inside a unit sphere.  The passed in RNG *must* be
-- an infinite list of 'Double's.
randomInUnitSphere :: RNG -> (Vec3, RNG)
randomInUnitSphere rng =
    let (x:y:z:tl) = rng
        vec = (x,y,z)
        len = squaredNorm vec
    in if len >= 1 then randomInUnitSphere tl else (vec, tl)


-- | Return a random point inside a 2D unit disk centered at the origin.
randomInUnitDisk :: RNG -> ((Double, Double), RNG)
randomInUnitDisk rng
    | normSq < 1 = (point, tl)
    | otherwise  = randomInUnitDisk tl
    where (x:y:tl) = rng
          point = (2 * x - 1, 2 * y - 1)
          normSq = fst point * fst point + snd point * snd point
