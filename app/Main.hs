import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import System.Random
import System.IO.Unsafe
import Graphics.Gloss.Interface.Pure.Display
import Data.List

-- Constants for snowflakes
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600
numberOfSnowflakes :: Int
numberOfSnowflakes = 100

type Snowflake = (Float, Float, Float, Float) -- (x, y, size, drift)
type State = (Int, Int, [Snowflake]) -- (season, counter, snowflakes)

-- Snowflake functions
generateSnowflakes :: StdGen -> [Snowflake]
generateSnowflakes gen = take numberOfSnowflakes $ zip4 xs ys sizes drifts
  where
    (genX, genRest1) = split gen
    (genY, genRest2) = split genRest1
    (genSize, genDrift) = split genRest2
    xs = randomRs (-fromIntegral windowWidth / 2, fromIntegral windowWidth / 2) genX
    ys = randomRs (0, fromIntegral windowHeight) genY
    sizes = randomRs (5, 15) genSize
    drifts = randomRs (-1, 1) genDrift

updateSnowflakes :: Float -> Float -> Bool -> Float -> [Snowflake] -> [Snowflake]
updateSnowflakes dt speed horizontal sizeScalar = map updateSnowflake
  where
    updateSnowflake (x, y, size, drift) =
      let newY = y - dt * (size * 5 * sizeScalar) * speed -- Larger snowflakes fall faster
          wrappedY = if newY < -fromIntegral windowHeight / 2
                     then fromIntegral windowHeight / 2
                     else newY
          newX = if horizontal then x + drift else x -- Apply horizontal drift or not
      in (newX, wrappedY, size, drift)

drawObjects :: Int -> [Snowflake] -> Picture
drawObjects season = Pictures . map (drawObject season)
  where
    drawObject :: Int -> Snowflake -> Picture
    drawObject season (x, y, size, _) =
      case season of
        0 -> -- Draw flowers for Spring
          Translate x y $
          Color (makeColor 1 0.8509 0.929 0.8) $ -- Green shades
          rotate (size * 10) $ -- Add some rotation for variety
          polygon [(-size, -size / 2), (0, size), (size, -size / 2)]
        1 -> -- Draw rain for summer
          Translate x y $
          Color (makeColor 0.5 0.5 0.7 0.8) $ -- Blue rain with slight transparency
          Line [(0, -size / 2), (0, size / 2)]  -- Vertical line for the raindrop
        2 -> -- Draw leaves for Autumn
          Translate x y $
          Color (makeColor 1.0 0.647 0.0 0.8) $ -- Orange shades
          rotate (size * 10) $ -- Add some rotation for variety
          polygon [(0,0),(0,size),(-size,size),((-0.4*size),1.3*size),(0,(2*size)),(0.4*size,1.3*size),(size,0),(0,size),(0,0)]
        3 -> -- Draw snowflakes for winter4
          Translate x y $
          Color white $
          circleSolid size

--(-size, -size / 2), (0, size), (size, -size / 2)

-- Seasonal scenes (your existing code) !!!EXISTIED CODE HERE BELOW!!!
drawTree :: Int -> Point -> Point -> Float -> String -> Picture
drawTree 0 _ _ _ _ = Blank  
drawTree depth (xa, ya) (xb, yb) angle seasontree =
    pictures [ 
      color (treeColor depth seasontree) $ polygon [
            (xa, ya),
            (xa + lddx, ya + lddy),
            (xa + ldx + lddx, ya + ldy + lddy),
            (xa + ldx, ya + ldy)
            ],
      color (treeColor depth seasontree) $ polygon [
            (xb, yb),
            (xb + rddx, yb + rddy),
            (xb + rdx + rddx, yb + rdy + rddy),
            (xb + rdx, yb + rdy)
            ],
      drawTree (depth - 1) (xa + ldx, ya + ldy) (xa + ldx + lddx, ya + ldy + lddy) angle seasontree,
      drawTree (depth - 1) (xb + rdx + rddx, yb + rdy + rddy) (xb + rdx, yb + rdy) angle seasontree
    ]
  where
    (ldx, ldy) = rotateV (0.5 * pi) (lddx, lddy)
    (lddx, lddy) = rotateV (angle / 180 * pi) ((cos (angle / 180 * pi)) * (xb - xa), (cos (angle / 180 * pi)) * (yb - ya))
    (rdx, rdy) = rotateV (-0.5 * pi) (rddx, rddy)
    (rddx, rddy) = rotateV (angle / 180 * pi - 0.5 * pi) ((sin (angle / 180 * pi)) * (xa - xb), (sin (angle / 180 * pi)) * (ya - yb))

-- Define tree color function
treeColor :: Int -> String -> Color
treeColor depth seasontree 
  | seasontree == "Spring" = makeColor (0.56) (0.76 + 0.8 * depthRatio) (0.52) 1
  | seasontree == "Summber" = makeColor (0.1) (0.67 + 0.8 * depthRatio) (0.04) 1
  | seasontree == "Autumn" = makeColor (0.55 + 0.8 * depthRatio) (0.1 + 0.5 * depthRatio) (0.1) 1
  | seasontree == "Winter" = makeColor (0.63 + 0.8 * depthRatio) (0.37 + 0.5 * depthRatio) (0) 1
  | otherwise = makeColor (0.0) (0.5 * depthRatio) (0.0) 1

  where
    maxDepth = 10  -- 你可以设置最大递归深度为 10 或其他值，以控制颜色变化的速度
    depthRatio = fromIntegral depth / fromIntegral maxDepth


-- 定义窗口大小和标题
window :: Display
window = InWindow "Seasons from the Window" (800, 600) (100, 100)

background :: Color
background = white


-- Define Color
brown :: Color
brown = makeColor 0.6 0.3 0.0 1.0


pink :: Color
pink = makeColor 1 0.8509 0.929 1

springGreen :: Color
springGreen = makeColor 0.59 0.65 0.29 1

springPink :: Color
springPink = makeColor 1 0.75 0.8 1

summerYellow :: Color
summerYellow = makeColor 1 0.87 0.28 1

summerBlue :: Color
summerBlue = makeColor 0.08 0.74 0.92 1

autumnYellow :: Color
autumnYellow = makeColor 0.74 0.71 0.41 1

autumnBrown :: Color
autumnBrown = makeColor 0.76 0.47 0.15 1

winterGray:: Color
winterGray = makeColor 0.83 0.83 0.83 1

winterBlack :: Color
winterBlack = makeColor 0.2 0.42 0.82 1


gray :: Color
gray = makeColor 0.86078 0.86078 0.86078 1

-- Draw window frame
windowFrame :: Picture
windowFrame = color brown $ pictures
  [ translate (-400) 0 $ rectangleSolid 80 600  
  , translate 400 0 $ rectangleSolid 80 600    
  , translate 0 (-200) $ rectangleSolid 800 40 
  , translate 0 200 $ rectangleSolid 800 40    
  , color (makeColor 0.86078 0.86078 0.86078 0.6) $ translate 0 260 $ rectangleSolid 720 80
  , color (makeColor 0.86078 0.86078 0.86078 0.6) $ translate 0 (-260) $ rectangleSolid 720 80
  ]

gradientBackground :: Color -> Color ->Picture
gradientBackground seasonColortop seasonCOlorbottom = Pictures [ translate 0 (fromIntegral y - fromIntegral windowHeight / 2)
                                $ color (makeGradient y seasonColortop seasonCOlorbottom) (rectangleSolid (fromIntegral windowWidth) 2)
                                | y <- [0..windowHeight]]


-- Generate gradient color
makeGradient :: Int -> Color -> Color -> Color
makeGradient y seasonColortop seasonCOlorbottom = mixColors (fromIntegral y / fromIntegral windowHeight)
                           (1 - fromIntegral y / fromIntegral windowHeight)
                           seasonColortop
                           seasonCOlorbottom


-- Draw spring scene
springScene :: Picture
springScene = pictures
  [ gradientBackground springGreen springPink
  ]

-- Draw summer scene
summerScene :: Picture
summerScene = pictures
  [ gradientBackground summerYellow summerBlue
  ]

-- Draw autumn scene
autumnScene :: Picture
autumnScene = pictures
  [ gradientBackground autumnYellow autumnBrown
  ]

-- Draw winter scene
winterScene :: Picture
winterScene = pictures
  [ gradientBackground winterGray winterBlack
  , color (white) $ translate 0 (-180) $ rectangleSolid 800 250 
  , color gray $ translate (200) (-100) $ circleSolid 60 
  , color gray $ translate (200) (-30) $ circleSolid 40  
  , color black $ translate (200) (-40) $ circleSolid 5
  , color black $ translate (185) (-20) $ circleSolid 5
  , color black $ translate (215) (-20) $ circleSolid 5
  ]


-- Return different scenes from each season
seasonScene :: Int -> Picture
seasonScene 0 = pictures [springScene,color brown $ polygon[(-200,-100),(-200,-60),(-160,-60),(-160,-100)], drawTree 2 (-200,-60) (-160,-60) 45 "Spring"]
seasonScene 1 = pictures [summerScene,color brown $ polygon[(-200,-100),(-200,0),(-100,0),(-100,-100)], drawTree 7 (-200,0) (-100,0) 55 "Summer"]
seasonScene 2 = pictures [autumnScene,color brown $ polygon[(-200,-100),(-200,0),(-100,0),(-100,-100)], drawTree 4 (-200,0) (-100,0) 53.13 "Autumn"]
seasonScene 3 = pictures [winterScene,color brown $ polygon[(-200,-100),(-200,0),(-100,0),(-100,-100)], drawTree 1 (-200,0) (-100,0) 53.13 "Winter"]
seasonScene _ = pictures [springScene,color brown $ polygon[(-200,-100),(-200,0),(-100,0),(-100,-100)], drawTree 2 (-200,-60) (-160,-60) 45 "Spring"]


-- Combine seasons and snowflakes
drawScene :: State -> Picture
drawScene (season, _, snowflakes) =
  pictures
    [ seasonScene season -- Existing seasonal scene
    , drawObjects season snowflakes -- Render snowflakes or leaves based on the season
    , windowFrame -- Window frame
    ]

-- Main function
main :: IO ()
main = do
  gen <- getStdGen
  let snowflakes = generateSnowflakes gen
  simulate
    window
    background
    60
    (0, 0, snowflakes) -- Initial state: season = 0, counter = 0, snowflakes = initial set
    render
    update
  where
    render state = drawScene state
    update _ dt (season, counter, snowflakes) =
      let newSnowflakes = updateSnowflakes dt (if season == 1 then 5.0 else 1.0) (season /= 1) (if season == 3 then 0.7 else 1.0) snowflakes
          newCounter = if counter >= 60 then 0 else counter + 1
          newSeason = if counter >= 60 then (season + 1) `mod` 4 else season
      in (newSeason, newCounter, newSnowflakes)
