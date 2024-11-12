  
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import System.Random()
import System.IO.Unsafe()
import Graphics.Gloss.Interface.Pure.Display()


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



-- 绘制窗户框架
windowFrame :: Picture
windowFrame = color brown $ pictures
  [ translate (-350) 0 $ rectangleSolid 100 800  -- 左边框
  , translate 350 0 $ rectangleSolid 100 800    -- 右边框
  , translate 0 (-200) $ rectangleSolid 600 40 -- 下边框
  , translate 0 200 $ rectangleSolid 600 40    -- 上边框

  ]

-- 绘制春天背景
springScene :: Picture
springScene = pictures
  [ color (light green) $ rectangleSolid 800 600
  ]

-- 绘制夏天背景
summerScene :: Picture
summerScene = pictures
  [ color (light blue) $ rectangleSolid 800 600
  ]

-- 绘制秋天背景
autumnScene :: Picture
autumnScene = pictures
  [ color (orange) $ rectangleSolid 800 600
  ]

-- 绘制冬天背景
winterScene :: Picture
winterScene = pictures
  [ color (light cyan) $ rectangleSolid 800 600
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

-- 绘制窗外四季景色
drawScene :: Int -> Picture
drawScene season = pictures
  [ seasonScene season
  , windowFrame
  ]
  

-- 主函数，轮流展示四季景色
main :: IO ()
main = simulate window background 1 (0, 0) render update
  where
    render (season, _) = drawScene (season `mod` 4)
    update _ _ (season, counter) = 
      if counter >= 1  -- Adjust this value to slow down further
      then ((season + 1) `mod` 4, 0)  -- Reset counter and change season
      else (season, counter + 1)  -- Increment counter only
