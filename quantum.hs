#!/usr/bin/env stack
-- stack script --resolver lts-22.11 --package containers
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Monad
import           Data.Complex
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as S
import           System.Environment
import           System.IO

-- Model --

type World = [Flow]

data Flow = Flow
    { configuration :: Configuration
    , amplitude     :: Amplitude
    }

data Configuration = Configuration
    { particles :: Set Particle
    , objects   :: Map Coord Object
    } deriving (Eq, Ord, Show)

data Object
    = MirrorRight     -- //            deflect
    | MirrorLeft      -- \\            - " -
    | SemiMirrorRight -- /             deflect and pass through
    | SemiMirrorLeft  -- \             - " -
    | Obstacle        -- #             block and destroy
    | Switch Bool     -- o|O           pass through and change own state
    | Detector Int    -- 0, 1, 2, ...  count and destroy
    deriving (Eq, Ord, Show)

type Amplitude = Complex Double

type Particle = (Coord, Coord)

type Coord = (Int, Int)

evolve :: World -> World
evolve = maybeCombine . concatMap processFlow

maybeCombine :: World -> World
maybeCombine
    = fmap (\(c, (_, a)) -> Flow c a)
    . sortOn (fst . snd)
    . M.toList
    . M.fromListWith (\(i, a) (_, a')-> (i, a + a'))
    . zipWith (\i (c,a) -> (c,(i,a))) [(1::Int)..]
    . fmap (\Flow{..} -> (configuration, amplitude))

processFlow :: Flow -> [Flow]
processFlow flow = processParticles (particles . configuration $ flow) flow

processParticles :: Set Particle -> Flow -> [Flow]
processParticles particles flow | S.null particles = [flow]
                                | otherwise        = foldr processParticle [flow] particles

processParticle :: Particle -> [Flow] -> [Flow]
processParticle particle@(_, position) = foldr processCollisionWithObject mempty
  where
      processCollisionWithObject :: Flow -> [Flow] -> [Flow]
      processCollisionWithObject flow = (<>) $ processCollision maybeObject flow
          where maybeObject = M.lookup position (objects . configuration $ flow)

      processCollision :: Maybe Object -> Flow -> [Flow]
      processCollision (Just MirrorRight) Flow{configuration=configuration@Configuration{..},..} =
          let newConfiguration = configuration
                  { particles = updateParticle particle newParticle particles
                  }
              [newParticle] = collide MirrorRight particle
          in [Flow newConfiguration (amplitude * iUnit)]

      processCollision (Just MirrorLeft) Flow{configuration=configuration@Configuration{..},..} =
          let newConfiguration = configuration
                  { particles = updateParticle particle newParticle particles
                  }
              [newParticle] = collide MirrorLeft particle
          in [Flow newConfiguration (amplitude * iUnit)]

      processCollision (Just SemiMirrorRight) Flow{configuration=configuration@Configuration{..},..} =
          let newConfigurationDirect = configuration
                  { particles = updateParticle particle newParticleDirect particles
                  }
              newConfigurationReflected = configuration
                  { particles = updateParticle particle newParticleReflected particles
                  }
              [newParticleDirect, newParticleReflected] = collide SemiMirrorRight particle
          in [Flow newConfigurationDirect amplitude, Flow newConfigurationReflected (amplitude * iUnit)]

      processCollision (Just SemiMirrorLeft) Flow{configuration=configuration@Configuration{..},..} =
          let newConfigurationDirect = configuration
                  { particles = updateParticle particle newParticleDirect particles
                  }
              newConfigurationReflected = configuration
                  { particles = updateParticle particle newParticleReflected particles
                  }
              [newParticleDirect, newParticleReflected] = collide SemiMirrorLeft particle
          in [Flow newConfigurationDirect amplitude, Flow newConfigurationReflected (amplitude * iUnit)]

      processCollision (Just Obstacle) Flow{configuration=configuration@Configuration{..},..} =
          let newConfiguration = configuration
                  { particles = S.delete particle particles
                  }
          in [Flow newConfiguration amplitude]

      processCollision (Just switch@(Switch b)) Flow{configuration=configuration@Configuration{..},..} =
          let newConfiguration = configuration
                  { particles = updateParticle particle newParticle particles
                  , objects = M.insert position (Switch $ not b) objects
                  }
              [newParticle] = collide switch particle
          in [Flow newConfiguration amplitude]

      processCollision (Just (Detector n)) Flow{configuration=configuration@Configuration{..},..} =
          let newConfiguration = configuration
                  { objects = M.insert position updatedDetector objects
                  , particles = S.delete particle particles
                  }
              updatedDetector = Detector $ if squaredModulus amplitude > 0 then n + 1 else n
          in [Flow newConfiguration amplitude]

      processCollision Nothing Flow{configuration=configuration@Configuration{..},..} =
          let newConfiguration = configuration
                  { particles = updateParticle particle newParticle particles
                  }
              [newParticle] = passThrough particle
          in [Flow newConfiguration amplitude]

      updateParticle oldParticle newParticle = S.insert newParticle . S.delete oldParticle

      iUnit = 0 :+ 1

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

passThrough :: Particle -> [Particle]
passThrough ((x1, y1), (x2, y2))
    | x1 < x2 && y1 == y2 = [((x2, y1), (x2 + 1, y2))]
    | x1 > x2 && y1 == y2 = [((x2, y1), (x2 - 1, y2))]
    | x1 == x2 && y1 < y2 = [((x1, y2), (x2, y2 + 1))]
    | x1 == x2 && y1 > y2 = [((x1, y2), (x2, y2 - 1))]
    | otherwise           = error $ "Invalid particle: " ++ show ((x1, y1), (x2, y2))

squaredModulus :: Amplitude -> Double
squaredModulus (a :+ b) = abs $ a^(2::Int) + b^(2::Int)

-- Render --

type View = [[String]]

renderWorld :: Coord -> World -> View
renderWorld dimentions@(width, height)
    = combineViewList
    . intersperse (spaceView 4 $ height + 2 {- caption -})
    . fmap (renderFlow dimentions)

renderFlow :: Coord -> Flow -> View
renderFlow dimentions Flow{..}
    = renderConfiguration dimentions configuration
    <> renderCaption dimentions amplitude

renderConfiguration :: Coord -> Configuration -> View
renderConfiguration (width, height) Configuration{..} = let
    worldMap = [ c | y <- [height-1,height-2..0], x <- [0..width-1], let c = showCell (x, y) ]
    showCell coord = fromMaybe ". " $ getObject coord <|> getParticle coord
    getParticle coord = showParticle . (coord, ) <$> M.lookup coord particlesMap
    getObject coord = showObject <$> M.lookup coord objects
    particlesMap = M.fromList . S.toList $ particles
    in chunksOf width worldMap

renderCaption :: Coord -> Amplitude -> View
renderCaption (width, _) amplitude = renderLine <$> [showAmplitude,showSquaredModulus]
    where
        renderLine showF = let text = showF amplitude in split (text <> filler text)
        split = chunksOf 2 . take realWidth
        filler text = concat $ replicate (realWidth - length text) "  "
        realWidth = width * 2

spaceView :: Int -> Int -> View
spaceView width heigh = replicate heigh $ replicate width "  "

showView :: View -> String
showView = unlines . fmap concat

combineViewList :: [View] -> View
combineViewList = foldl1 combineViews

combineViews :: View -> View -> View
combineViews = zipWith (<>)

-- Show --

showParticle :: Particle -> String
showParticle ((x1, y1), (x2, y2))
    | x1 == x2 && y1 < y2 = "^ "
    | x1 == x2 && y1 > y2 = "v "
    | x1 < x2 && y1 == y2 = "> "
    | x1 > x2 && y1 == y2 = "< "
    | otherwise           = error $ "Invalid particle: " ++ show ((x1, y1), (x2, y2))

showObject :: Object -> String
showObject = \case
    MirrorRight     -> "//"
    MirrorLeft      -> "\\\\"
    SemiMirrorRight -> "/ "
    SemiMirrorLeft  -> "\\ "
    Obstacle        -> "# "
    Switch b        -> if b then "O " else "o "
    Detector n      -> show (n `mod` 10) <> " "

showAmplitude :: Amplitude -> String
showAmplitude (a :+ b) = "a: " <> show (round a) ++ " + " ++ show (round b) ++ "i"

showSquaredModulus :: Amplitude -> String
showSquaredModulus a = "sm: " <> show (squaredModulus a)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = step xs []
    where
        step input output = case splitAt n input of
            (c, []) -> c : output
            (c, cs) -> c : step cs output


-- Worlds (Initial Configurations) --

initialAmplitude :: Amplitude
initialAmplitude = (-1) :+ 0

{-
      D
      |
      ^
      |
 -->--/-->--D

-}

world1 :: World
world1 =
    [ Flow
      { configuration = Configuration
          { particles = S.fromList
              [ ((0, 0), (1, 0))
              ]
          , objects = M.fromList
              [ ((4, 0), SemiMirrorRight)
              , ((8, 0), Detector 0)
              , ((4, 4), Detector 0)
              ]
          }
      , amplitude = initialAmplitude
      }
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

world2 :: World
world2 =
    [ Flow
      { configuration = Configuration
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
      , amplitude = initialAmplitude
      }
    ]

{-
            D
            |
            ^
            |
     //--#--/->-D
      |     |
      ^     ^
      |     |
 -->--/-->--//

-}

world3 :: World
world3 =
    [ Flow
      { configuration = Configuration
          { particles = S.fromList
              [ ((0, 0), (1, 0))
              ]
          , objects = M.fromList
              [ ((4, 0), SemiMirrorRight)
              , ((8, 0), MirrorRight)
              , ((4, 4), MirrorRight)
              , ((6, 4), Obstacle)
              , ((8, 4), SemiMirrorRight)
              , ((12, 4), Detector 0)
              , ((8, 8), Detector 0)
              ]
          }
      , amplitude = initialAmplitude
      }
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
 -->--/--o--//

-}

world4 :: World
world4 =
    [ Flow
      { configuration = Configuration
          { particles = S.fromList
              [ ((0, 0), (1, 0))
              ]
          , objects = M.fromList
              [ ((4, 0), SemiMirrorRight)
              , ((6, 0), Switch False)
              , ((8, 0), MirrorRight)
              , ((4, 4), MirrorRight)
              , ((8, 4), SemiMirrorRight)
              , ((12, 4), Detector 0)
              , ((8, 8), Detector 0)
              ]
          }
      , amplitude = initialAmplitude
      }
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

world5 :: World
world5 =
    [ Flow
      { configuration = Configuration
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
      , amplitude = initialAmplitude
      }
    ]

-- IO --

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

main :: IO ()
main = do
    let initWorlds = [world1 , world2 , world3 , world4 , world5]

    args <- getArgs
    let idx = case args of
            [index] -> read index
            _       -> error $ "Usage: ./quantum.hs <index>, where index is [1.."
                <> show (length initWorlds) <> "]"
                <> "\nLeft/right arrow keys to navigate past/future, 'q' to quit"

    let world = initWorlds !! (idx - 1)
        worlds = aliveWorlds <> [deadWorld]
        (aliveWorlds,deadWorld:_) = span alive $ iterate evolve world
        alive =  not . all (S.null . particles . configuration)
        width = dim fst world
        height = dim snd world
        dim f = (+ 1) . maximum . concatMap (fmap f . M.keys . objects . configuration)

        evolution index = do
            putStr "\ESC[2J"
            putStrLn $ showView . renderWorld (width, height) . (worlds !!) $ index
            key <- getKey
            case key of
                "q"      -> error "Quit"
                "\ESC"   -> error "Quit"
                "\ESC[D" -> evolution (dec index)
                "\DEL"   -> evolution (dec index)
                _        -> evolution (inc index)
            where
                size = length worlds
                inc i | i == size - 1 = 0
                      | otherwise     = i + 1
                dec i | i == 0 = size - 1
                      | otherwise     = i - 1

    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBuffering stdout NoBuffering

    forever $ evolution 0
