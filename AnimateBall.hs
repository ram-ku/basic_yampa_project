module AnimateBall where

import File1
import Animator
import Graphics.Blank

-- set browser to: http://localhost:3000/

main :: IO ()
--main = blankCanvas 3000 $ \ context -> loop context $ moving ball1
main = blankCanvas 3000 $ \ context -> loop context $ bouncingBall ball1
loop context (n:ns) = do{
						send context $ do {
							render (renderBall1 n) };
						loop context ns}

renderBall1 :: Ball -> Canvas ()
renderBall1  b = do {
					x <- scaleX (ballPosX b);
					y <- scaleY (ballPosY b);
					r <- scaleLength (ballRad b);
					circle x y r "Blue";
					--renderBall xs 
					}