  
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
-- import Graphics.Gloss
-- import Graphics.Gloss.Data.Vector
  
-- -- Main function to display the tree
-- main :: IO ()
-- main = display (InWindow "Pythagorean Tree" (800, 800) (10, 10)) white (pictures [color red $ circle 2, polygon[(0,0),(0,50),(50,50),(50,0)], drawTree 10 (0,50) (50,50) 53.13])

-- Function to draw the tree fractal
drawTree :: Int -> Point -> Point -> Float -> Picture
drawTree 0 _ _ _ = Blank  -- Base case: no more recursion when depth is 0
drawTree depth (xa , ya) (xb , yb) angle =
    pictures[ 
      color white $ polygon[
            (xa , ya),
            (xa + lddx, ya + lddy),
            (xa + ldx + lddx , ya + ldy + lddy),
            (xa + ldx , ya + ldy)
            ],
      color white $ polygon[
            (xb , yb),
            (xb + rddx , yb + rddy),
            (xb + rdx + rddx , yb + rdy + rddy),
            (xb + rdx , yb + rdy)
            ],
      drawTree (depth - 1) (xa + ldx, ya + ldy) (xa + ldx + lddx , ya + ldy + lddy) angle,
      drawTree (depth - 1) (xb + rdx + rddx , yb + rdy + rddy) (xb + rdx, yb + rdy) angle]
-- 
      where
            (ldx , ldy) = rotateV (0.5 * pi) (lddx, lddy)
            (lddx , lddy) = rotateV (angle / 180 * pi) ((cos(angle / 180 * pi))*(xb-xa),(cos(angle / 180 * pi))*(yb-ya))
            (rdx , rdy) = rotateV (-0.5 * pi) (rddx, rddy)
            (rddx , rddy) = rotateV (angle / 180 * pi - 0.5 * pi) ((sin(angle / 180 * pi)) * (xa - xb) , (sin(angle / 180 * pi)) * (ya - yb))


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
  , color white $ translate (-150) (-150) $ circleSolid 60 -- 雪人身体
  , color white $ translate (-150) (-80) $ circleSolid 40  -- 雪人头部
  , color (white) $ translate 0 (-200) $ rectangleSolid 800 20 -- 积雪
  ]

-- 根据不同季节返回不同的场景
seasonScene :: Int -> Picture
seasonScene 0 = pictures [ springScene,color white $ polygon[(0,0),(0,50),(50,50),(50,0)], drawTree 2 (0,50) (50,50) 45]
seasonScene 1 = pictures [summerScene,color white $ polygon[(0,0),(0,50),(50,50),(50,0)], drawTree 6 (0,50) (50,50) 53.13]
seasonScene 2 = pictures [autumnScene,color white $ polygon[(0,0),(0,50),(50,50),(50,0)], drawTree 4 (0,50) (50,50) 53.13]
seasonScene 3 = pictures [winterScene,color white $ polygon[(0,0),(0,50),(50,50),(50,0)], drawTree 1 (0,50) (50,50) 53.13]
seasonScene _ = pictures [springScene,color white $ polygon[(0,0),(0,50),(50,50),(50,0)], drawTree 2 (0,50) (50,50) 45]

-- 绘制窗外四季景色
drawScene :: Int -> Picture
drawScene season = pictures
  [ seasonScene season
  , windowFrame
  ]
  

-- 主函数，轮流展示四季景色
main :: IO ()
main = simulate window background 20 0 render update
  where
    render season = drawScene (season `mod` 4)
    update _ _ season = (season + 1) `mod` 4
