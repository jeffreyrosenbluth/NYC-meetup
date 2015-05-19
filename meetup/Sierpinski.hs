sierpinski 1 = triangle 1 -- hexagon 1 ...
sierpinski n =    s
                 ===
              (s ||| s) # centerX
  where s = sierpinski (n-1)
