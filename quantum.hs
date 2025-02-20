#!/usr/bin/env stack
-- stack script --resolver lts-22.11 --package containers
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Concurrent  (threadDelay)
import           Control.Monad
import           Data.Complex
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Tuple
import           System.Environment
import           System.IO

type World = [Flow]

type Flow = (Configuration, Amplitude)

data Configuration = Configuration
    { particles :: Set Particle
    , objects   :: Map Position Object
    } deriving (Eq, Ord, Show)

data Object
    = MirrorRight     -- //
    | MirrorLeft      -- \\
    | SemiMirrorRight -- /
    | SemiMirrorLeft  -- \
    | Switch Bool     -- o|O
    | Detector Int    -- 0, 1, 2, ...
    deriving (Eq, Ord, Show)

type Amplitude = Complex Double

type Particle = (Position, Position)

type Position = (Double, Double)

evolve :: World -> World
evolve = M.fromListWith (+) . concatMap processConfiguration . M.toList

processConfiguration :: Flow -> [Flow]
processConfiguration flow@(configuration@Configuration{..}, amplitude) = processParticles particles
    where
        processParticles :: Set Particle -> [Flow]
        processParticles particles | S.null particles = [flow]
                                   | otherwise        = concatMap processParticle particles
        processParticle :: Particle -> [Flow]
        processParticle particle@(_, position) = collideWith $ M.lookup position objects
          where
              collideWith :: Maybe Object -> [Flow]
              collideWith (Just MirrorRight) =
                  let newConfiguration = configuration
                          { particles = S.insert newParticle . S.delete particle $ particles
                          }
                      [newParticle] = collide MirrorRight particle
                  in [(newConfiguration, amplitude * i)]

              collideWith (Just MirrorLeft) =
                  let newConfiguration = configuration
                          { particles = S.insert newParticle . S.delete particle $ particles
                          }
                      [newParticle] = collide MirrorLeft particle
                  in [(newConfiguration, amplitude * i)]

              collideWith (Just SemiMirrorRight) =
                  let newConfigurationDirect = configuration
                          { particles = S.insert newParticleDirect . S.delete particle $ particles
                          }
                      newConfigurationReflected = configuration
                          { particles = S.insert newParticleReflected . S.delete particle $ particles
                          }
                      [newParticleDirect, newParticleReflected] = collide SemiMirrorRight particle
                  in [(newConfigurationDirect, amplitude), (newConfigurationReflected, amplitude * i)]

              collideWith (Just SemiMirrorLeft) =
                  let newConfigurationDirect = configuration
                          { particles = S.insert newParticleDirect . S.delete particle $ particles
                          }
                      newConfigurationReflected = configuration
                          { particles = S.insert newParticleReflected . S.delete particle $ particles
                          }
                      [newParticleDirect, newParticleReflected] = collide SemiMirrorLeft particle
                  in [(newConfigurationDirect, amplitude), (newConfigurationReflected, amplitude * i)]

              collideWith (Just switch@(Switch b)) =
                  let newConfiguration = configuration
                          { particles = S.insert newParticle . S.delete particle $ particles
                          , objects = M.insert position (Switch $ not b) objects
                          }
                      [newParticle] = collide switch particle
                  in [(newConfiguration, amplitude)]

              collideWith (Just (Detector n)) =
                  let newConfiguration = configuration
                          { objects = M.insert position updatedDetector objects
                          , particles = S.delete particle particles
                          }
                      updatedDetector = Detector $ if squaredModulus amplitude > 0 then n + 1 else n
                  in [(newConfiguration, amplitude)]

              collideWith Nothing =
                  let newConfiguration = configuration
                          { particles = S.insert newParticle . S.delete particle $ particles
                          }
                      [newParticle] = passThrough particle
                  in [(newConfiguration, amplitude)]

collide :: Object -> Particle -> [Particle]
collide MirrorRight ((x1, y1), (x2, y2)) =
    let particle
            | x1 < x2 && y1 == y2 = ((x2, y2), (x2, y2 + 1))
            | x1 > x2 && y1 == y2 = ((x2, y2), (x2, y2 - 1))
            | x1 == x2 && y1 < y2 = ((x2, y2), (x2 + 1, y2))
            | x1 == x2 && y1 > y2 = ((x2, y2), (x2 - 1, y2))
            | otherwise           = error $ "Invalid particle: " ++ show ((x1, y1), (x2, y2))
    in [particle]

collide MirrorLeft ((x1, y1), (x2, y2)) =
    let particle
            | x1 < x2 && y1 == y2 = ((x2, y2), (x2, y2 - 1))
            | x1 > x2 && y1 == y2 = ((x2, y2), (x2, y2 + 1))
            | x1 == x2 && y1 < y2 = ((x2, y2), (x2 - 1, y2))
            | x1 == x2 && y1 > y2 = ((x2, y2), (x2 + 1, y2))
            | otherwise           = error $ "Invalid particle: " ++ show ((x1, y1), (x2, y2))
    in [particle]

collide SemiMirrorRight ((x1, y1), (x2, y2)) =
    let particles
            | x1 < x2 && y1 == y2 = [((x2, y1), (x2 + 1, y2)), ((x2, y2), (x2, y2 + 1))]
            | x1 > x2 && y1 == y2 = [((x2, y1), (x2 - 1, y2)), ((x2, y2), (x2, y2 - 1))]
            | x1 == x2 && y1 < y2 = [((x1, y2), (x2, y2 + 1)), ((x2, y2), (x2 + 1, y2))]
            | x1 == x2 && y1 > y2 = [((x1, y2), (x2, y2 - 1)), ((x2, y2), (x2 - 1, y2))]
            | otherwise           = error $ "Invalid particle: " ++ show ((x1, y1), (x2, y2))
    in particles

collide SemiMirrorLeft ((x1, y1), (x2, y2)) =
    let particles
            | x1 < x2 && y1 == y2 = [((x2, y1), (x2 + 1, y2)), ((x2, y2), (x2, y2 - 1))]
            | x1 > x2 && y1 == y2 = [((x2, y1), (x2 - 1, y2)), ((x2, y2), (x2, y2 + 1))]
            | x1 == x2 && y1 < y2 = [((x1, y2), (x2, y2 + 1)), ((x2, y2), (x2 - 1, y2))]
            | x1 == x2 && y1 > y2 = [((x1, y2), (x2, y2 - 1)), ((x2, y2), (x2 + 1, y2))]
            | otherwise           = error $ "Invalid particle: " ++ show ((x1, y1), (x2, y2))
    in particles

collide (Switch _) particle = passThrough particle

collide object _ = error $ "Object is not supported: " <> show object

passThrough ((x1, y1), (x2, y2))
    | x1 < x2 && y1 == y2 = [((x2, y1), (x2 + 1, y2))]
    | x1 > x2 && y1 == y2 = [((x2, y1), (x2 - 1, y2))]
    | x1 == x2 && y1 < y2 = [((x1, y2), (x2, y2 + 1))]
    | x1 == x2 && y1 > y2 = [((x1, y2), (x2, y2 - 1))]

squaredModulus :: Amplitude -> Double
squaredModulus (a :+ b) = abs $ a^2 + b^2

i :: Amplitude
i = 0 :+ 1

main = do
    let worlds = [(world1, 5, 5, 9), (world2, 9, 5, 9), (world3, 10, 10, 5), (world4, 12, 12, 5)]

    args <- getArgs
    let index = case args of
            [index] -> read index
            _       -> error $ "Usage: ./quantum.hs <index>, where index is [0.." <> show (length worlds - 1) <> "]"

    let (world, width, height, time) = worlds !! index
        evolution = take time (iterate evolve world)

--    traverse print evolution

    hSetBuffering stdout NoBuffering
    hSetBinaryMode stdout True
    forever $ do
        forM_ evolution $ \world -> do
            putStr "\ESC[2J"
            putStrLn $ renderWorld width height world
            threadDelay 500000
        threadDelay 2000000

-- Presentation --

type View = [[String]]

spaceView width heigh = replicate heigh $ replicate width "  "

showView :: View -> String
showView = unlines . fmap concat

combineViewList :: [View] -> View
combineViewList = foldl1 combineViews

combineViews :: View -> View -> View
combineViews = zipWith (<>)

-- Render --

renderWorld :: Double -> Double -> World -> String
renderWorld width height
    = showView
    . combineViewList
    . intersperse (spaceView 4 (round height))
    . fmap (renderConfiguration width height)
    . M.keys

renderConfiguration :: Double -> Double -> Configuration -> View
renderConfiguration width height Configuration{..} = let
    map = [ c | y <- [height-1,height-2..0], x <- [0..width-1], let c = showCell (x, y) ]
    showCell coord = fromMaybe ". " $ getObject coord <|> getParticle coord
    getParticle coord = showParticle . (coord, ) <$> M.lookup coord particlesMap
    getObject coord = showObject <$> M.lookup coord objects
    particlesMap = M.fromList . S.toList $ particles
    in chunksOf (round width) map

showParticle :: Particle -> String
showParticle ((x1, y1), (x2, y2))
    | x1 == x2 && y1 < y2 = "^ "
    | x1 == x2 && y1 > y2 = "v "
    | x1 < x2 && y1 == y2 = "> "
    | x1 > x2 && y1 == y2 = "< "
    | otherwise           = error $ "Invalid particle: " ++ show ((x1, y1), (x2, y2))

showObject = \case
    MirrorRight     -> "//"
    MirrorLeft      -> "\\\\"
    SemiMirrorRight -> "/ "
    SemiMirrorLeft  -> "\\ "
    Switch b        -> if b then "O " else "o "
    Detector n      -> show (n `mod` 10) <> " "

--renderAmplitude :: Amplitude -> String
--renderAmplitude (a :+ b) = show a ++ " + " ++ show b ++ "i"

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = step xs []
    where
        step input output = case splitAt n input of
            (c, []) -> c : output
            (c, cs) -> c : step cs output

-- Worlds --

initialAmplitude :: Amplitude
initialAmplitude = (-1) :+ 0

{-
      D
      |
      ^
      |
 -->--//

-}

world1 :: World
world1 =
    M.fromList
    [ ( Configuration
        { particles = S.fromList
            [ ((0, 0), (1, 0))
            ]
        , objects = M.fromList
            [ ((4, 0), MirrorRight)
            , ((4, 4), Detector 0)
            ]
        }
      , initialAmplitude
      )
    ]

{-
      D
      |
      ^
      |
 -->--/-->--D

-}

world2 :: World
world2 =
    M.fromList
    [ ( Configuration
        { particles = S.fromList
            [ ((0, 0), (1, 0))
            ]
        , objects = M.fromList
            [ ((4, 0), SemiMirrorRight)
            , ((8, 0), Detector 0)
            , ((4, 4), Detector 0)
            ]
        }
      , initialAmplitude
      )
    ]

{-
            D
            |
            ^
            |
     //-->--/->-D
      |     |
      ^     ^
      |     |
 -->--/-->--//

-}

world3 :: World
world3 =
    M.fromList
    [ ( Configuration
        { particles = S.fromList
            [ ((0, 0), (1, 0))
            ]
        , objects = M.fromList
            [ ((4, 0), SemiMirrorRight)
            , ((8, 0), MirrorRight)
            , ((4, 4), MirrorRight)
            , ((8, 4), SemiMirrorRight)
            , ((12, 4), Detector 0)
            , ((8, 8), Detector 0)
            ]
        }
      , initialAmplitude
      )
    ]

{-
      D
      |
      ^
      |
 -->--/-->--D
      |
      ^
      |

-}

world4 :: World
world4 =
    M.fromList
    [ ( Configuration
        { particles = S.fromList
            [ ((0, 4), (1, 4))
            , ((4, 0), (4, 1))
            ]
        , objects = M.fromList
            [ ((4, 4), SemiMirrorRight)
            , ((4, 8), Detector 0)
            , ((8, 4), Detector 0)
            ]
        }
      , initialAmplitude
      )
    ]

