# Seasons Through the Window: Variations of the Pythagoras Tree

This project is a Haskell program that creates a visual simulation of seasonal scenes with falling snowflakes using the **Gloss** graphics library. It combines dynamic visual effects, such as snowfall, with static seasonal Pythagoras tree representations.

![demo-gif](/tree-demo.gif)

## Table of Contents

1. Collaborators
2. Overview
3. Features
4. How to Run
5. Dependencies


---

## Collaborators
Kecheng Jin (Tree builder)   
Hagan Wang (Special effects)   
Brian Qian (Special effect + Documentation)
Yirou Dong (Background)

---

## Overview

This project explores the Pythagoras Tree and its connection to the changing seasons. Each season is represented through aesthetic transformations—modifying colors and structure—to evoke the distinct characteristics of spring, summer, autumn, and winter.

By viewing these variations "through the window," the project offers a creative interpretation of how mathematical forms can adapt to reflect and connect with the temporal and visual qualities of the natural world.

---

## Features

- **Dynamic snowfall:** Snowflakes fall at varying speeds, directions, and drifts. The larger the snowflake, the closer it is to the window, so it falls faster, and vice versa. This creates different 3D layering for the scene, which vivifies the environment.
- **Seasonal transitions:** Cycles through Spring, Summer, Autumn, and Winter every few seconds. The tree will be in different states (seedling → growing up → thriving) and allocated to each season, to show the process of a tree growing up mathematically. The snowflakes will change to different styles to match the season. For example, in summer, it will switch to rainfall; in autumn, it will be falling leaves; and in spring, it will be flowers. The particles' positions in the special effect will remain the same even when the season changes, to highlight the transition between seasons.
- **Gradient backgrounds:** Smooth gradient effects represent each season and also the color of the leaves, which enrich the color tone and improve the visual effect.
- **Window frame:** Adds a realistic touch by drawing a frame around the scenes and a transparent color block overlay on the top and bottom of the window, to simulate the transparent glass of the window.
- **Interactive simulation:** Refreshes at 60 frames per second for smooth animations.

---

## How to Run

- **Install Haskell:**
[Link to download Haskell](https://www.haskell.org/downloads/)
Ensure Haskell is installed and Gloss is available as a package

- **Run the Program:**
Run the terminal in the stored folder:

cabal build
cabal run

-**Enjoy the Simulation:**

 Watch as the seasons change and snowflakes fall dynamically.

---

## Dependencies

Haskell: Functional programming language.
Gloss: Graphics library for simple 2D visualizations.





