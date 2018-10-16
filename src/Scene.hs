module Scene
( world
, world1
, world2
, world3
, skyColor1
, skyColor2
) where

import Trace

type World = (Camera, [Primitive])

-- | List of primitives making up world, and also an interesting viewpoint
world :: World
world = world3


-- | The color of the sky at the top of the world.
skyColor1 :: Vec3
skyColor1 = (0.5, 0.7, 1.0)


-- | The color of the sky at the bottom of the world.
skyColor2 :: Vec3
skyColor2 = (1.0, 1.0, 1.0)


----------------------------------------------------------------------------
-- The following are several example scenes that can be rendered with the --
-- ray-tracer.  Feel free to create your own.                             --
----------------------------------------------------------------------------

-- | This one is from Ch. 11 of Peter Shirley's book.  It is helpful in
-- demonstrating depth of field.
world1 :: World
world1 =
    (camera, [sphere1, sphere2, sphere3, ground])
    where ground  = makeSphere (0.0,-100.5,-1.0) 100.0 (makeDiffuse (0.8, 0.8, 0.0))
          sphere1 = makeSphere (0.0, 0.0, -1.0) 0.5 (makeDiffuse (0.1, 0.2, 0.5))
          sphere2 = makeSphere (1.0, 0.0, -1.0) 0.5 (makeMetallic (0.8, 0.6, 0.2) 0.3)
          sphere3 = makeSphere (-1.0, 0.0, -1.0) 0.5 (makeRefractive 1.5)
          camera = makeCamera (3, 3, 2) (0, 0, -1) (0, 1, 0) 0.35 (200, 100) 2


-- | Create several random spheres with random materials.
randomScene :: RNG -> [Primitive]
randomScene rng = fst (foldl randomObject ([], rng)
                             [(a, b) | a <- [-5..4], b <- [-5..4]])

-- | Create a random sphere with a random material.
randomObject :: ([Primitive], RNG) -> (Double, Double) -> ([Primitive], RNG)
randomObject (acc, rng) (basePosX, basePosZ) =
    let (offsetX:offsetZ:newRng) = rng
        center = (basePosX + 0.9 * offsetX, 0.2, basePosZ + 0.9 * offsetZ)
        (material, newRng2) = randomMaterial newRng
    in ((makeSphere center 0.2 material):acc, newRng2)

-- | Choose a random material
randomMaterial :: RNG -> (Material, RNG)
randomMaterial rng
    | random < 0.8  = (makeDiffuse (matRnd1 * matRnd2, matRnd3 * matRnd4,
                                    matRnd5 * matRnd6),
                       tl)
    | random < 0.95 = (makeMetallic (0.5 .* ((1, 1, 1) + (matRnd1, matRnd2, matRnd3)))
                                    (0.5 * matRnd4), tl)
    | otherwise     = (makeRefractive 1.5, tl)
    where (random:matRnd1:matRnd2:matRnd3:matRnd4:matRnd5:matRnd6:tl) = rng


-- | Varied scene with lots of objects.  This scene is good for benchmarking.
world2 :: World
world2 =
    (camera, [ground, sphere1, sphere2, sphere3] ++ randomScene (mkRNG 7))
    where camera = makeCamera (13, 2, 3) (0, 0, 0) (0, 1, 0) 0.35 (300, 200) 0.2
          ground = makeSphere (0, -1000, 0) 1000 (makeDiffuse (0.5, 0.5, 0.5))
          sphere1 = makeSphere (0, 1, 0) 1 (makeRefractive 1.5)
          sphere2 = makeSphere (-4, 1, 0) 1 (makeDiffuse (0.4, 0.2, 0.1))
          sphere3 = makeSphere (4, 1, 0) 1 (makeMetallic (0.7, 0.6, 0.5) 0)


-- | An interesting feature of our material system is that if we set the albedo
-- color to be greater than 1 in at least one component, it will actually *emit*
-- light.
world3 :: World
world3 =
    (camera, [sphere1, sphere2, sphere3, ground])
    where ground  = makeSphere (0.0,-100.5,-1.0) 100.0 (makeDiffuse (0.8, 0.8, 0.0))
          sphere1 = makeSphere (0.0, 0.0, -1.0) 0.5 (makeDiffuse (16, 8, 0.5))
          sphere2 = makeSphere (1.0, 0.0, -1.0) 0.5 (makeMetallic (0.8, 0.6, 0.2) 0.3)
          sphere3 = makeSphere (-1.0, 0.0, -1.0) 0.5 (makeRefractive 1.5)
          camera = makeCamera (3, 3, 2) (0, 0, -1) (0, 1, 0) 0.35 (200, 100) 0.01
