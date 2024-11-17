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

updateSnowflakes :: Float -> [Snowflake] -> [Snowflake]
updateSnowflakes dt = map updateSnowflake
  where
    updateSnowflake (x, y, size, drift) =
      let newY = y - dt * (size * 5) -- Larger snowflakes fall faster
          wrappedY = if newY < -fromIntegral windowHeight / 2
                     then fromIntegral windowHeight / 2
                     else newY
          newX = x + drift -- Apply horizontal drift
      in (newX, wrappedY, size, drift)

drawSnowflakes :: [Snowflake] -> Picture
drawSnowflakes = Pictures . map drawSnowflake
  where
    drawSnowflake (x, y, size, _) = Translate x y $ Color white $ circleSolid size

-- Seasonal scenes (your existing code) !!!EXISTIED CODE HERE BELOW!!!
drawTree :: Int -> Point -> Point -> Float -> String -> Picture
drawTree 0 _ _ _ _ = Blank  -- 基本情况：深度为 0 时停止递归
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
    -- 计算左、右分支的偏移量
    (ldx, ldy) = rotateV (0.5 * pi) (lddx, lddy)
    (lddx, lddy) = rotateV (angle / 180 * pi) ((cos (angle / 180 * pi)) * (xb - xa), (cos (angle / 180 * pi)) * (yb - ya))
    (rdx, rdy) = rotateV (-0.5 * pi) (rddx, rddy)
    (rddx, rddy) = rotateV (angle / 180 * pi - 0.5 * pi) ((sin (angle / 180 * pi)) * (xa - xb), (sin (angle / 180 * pi)) * (ya - yb))

-- 定义颜色渐变函数，根据深度控制颜色变化
treeColor :: Int -> String -> Color
treeColor depth seasontree 
  | seasontree == "Spring" = makeColor (0.0) (0.4 + 0.8 * depthRatio) (0.0) 1
  | seasontree == "Summber" = makeColor (0.0) (0.8 + 0.8 * depthRatio) (0.0) 1
  | seasontree == "Autumn" = makeColor (0.8 * depthRatio) (0.5 * depthRatio) (0.0) 1
  | seasontree == "Winter" = makeColor (0.2 + 0.8 * depthRatio) (0.5 * depthRatio) (0.0) 1
  | otherwise = makeColor (0.0) (0.5 * depthRatio) (0.0) 1
  where
    maxDepth = 10  -- 你可以设置最大递归深度为 10 或其他值，以控制颜色变化的速度
    depthRatio = fromIntegral depth / fromIntegral maxDepth


-- 定义窗口大小和标题
window :: Display
window = InWindow "Seasons from the Window" (800, 600) (100, 100)

background :: Color
background = white


-- 定义颜色
brown :: Color
brown = makeColor 0.6 0.3 0.0 1.0

pink :: Color
pink = makeColor 1.0 0.75 0.8 1.0

darkGreen :: Color
darkGreen = makeColor 0.0 0.5 0.0 1.0

purple :: Color
purple = makeColor 0.5 0 0.5 1

-- 绘制窗户框架
windowFrame :: Picture
windowFrame = color brown $ pictures
  [ translate (-350) 0 $ rectangleSolid 100 800  -- 左边框
  , translate 350 0 $ rectangleSolid 100 800    -- 右边框
  , translate 0 (-200) $ rectangleSolid 600 40 -- 下边框
  , translate 0 200 $ rectangleSolid 600 40    -- 上边框

  ]



gradientBackground :: Color -> Color ->Picture
gradientBackground seasonColortop seasonCOlorbottom = Pictures [ translate 0 (fromIntegral y - fromIntegral windowHeight / 2)
                                $ color (makeGradient y seasonColortop seasonCOlorbottom) (rectangleSolid (fromIntegral windowWidth) 2)
                                | y <- [0..windowHeight]]

-- 生成渐变颜色
makeGradient :: Int -> Color -> Color -> Color
makeGradient y seasonColortop seasonCOlorbottom = mixColors (fromIntegral y / fromIntegral windowHeight)
                           (1 - fromIntegral y / fromIntegral windowHeight)
                           seasonColortop
                           seasonCOlorbottom


-- 绘制春天背景

springScene :: Picture
springScene = pictures
  [ gradientBackground yellow green
  ]

-- 绘制夏天背景
summerScene :: Picture
summerScene = pictures
  [ gradientBackground (makeColor 0.56 0.85 1 1)  (makeColor 0.13 0.5 1 1)
  ]

-- 绘制秋天背景
autumnScene :: Picture
autumnScene = pictures
  [ gradientBackground yellow orange
  ]

-- 绘制冬天背景
winterScene :: Picture
winterScene = pictures
  [ gradientBackground pink purple
  , color white $ translate (200) (-100) $ circleSolid 60 -- 雪人身体
  , color white $ translate (200) (-30) $ circleSolid 40  -- 雪人头部
  , color (white) $ translate 0 (-170) $ rectangleSolid 800 20 -- 积雪
  ]

-- 根据不同季节返回不同的场景
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
    , drawSnowflakes snowflakes -- Draw snowflakes
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
      let newSnowflakes = updateSnowflakes dt snowflakes
          newCounter = if counter >= 60 then 0 else counter + 1
          newSeason = if counter >= 60 then (season + 1) `mod` 4 else season
      in (newSeason, newCounter, newSnowflakes)