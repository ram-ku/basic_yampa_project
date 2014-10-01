module File1 where

type Signal a = [a]
type Time=Int
data Event a = NoEvent | Event a deriving (Show , Eq)
type SF a b = Signal a -> Signal b

constant:: a -> Signal a
constant x = (repeat x)

identity :: Signal a -> Signal a
identity x= x

edge:: SF Bool (Event())
edge [] = []
edge [b] = [NoEvent]
edge (b:bs) = (abc:(edge bs))
	where abc = if(b==False && head(bs) == True) then Event() else NoEvent

notyet:: SF (Event a) (Event a)
notyet (a:as) = (NoEvent:as)
	where abc = NoEvent

never::Signal(Event a)
never = (repeat NoEvent)

repeatedly :: Time -> b -> SF a (Event b)
repeatedly q b = \l -> [Event b] ++ (take q (repeat NoEvent))  ++ (repeatedly q b l)

hold :: a -> SF (Event a) a
hold a (b:bs) = (abc b: hold a bs)
    where abc NoEvent = a
	  abc x = extfrm x
		where extfrm (Event x) = x

after :: Time -> b -> SF a (Event b)
after q x (c:cs)= take q (repeat NoEvent) ++ [Event x] ++ (repeat NoEvent)


integrate :: Num a => SF a a
integrate a = integral 0 a

integral :: Num a => a -> SF a a
integral a (b:bs) = ((a+b) : integral (a+b) bs)
integral a [] = [a]

iIntegral :: Num a => a -> SF a a
iIntegral x a =  map (\y -> x + y) (integrate a)

now:: b -> SF a (Event b)
now b _ = (Event b : repeat(NoEvent))

delay::Time -> a -> SF a a
delay t ax a= (take t (repeat ax)) ++ a

arr :: (a -> b) -> SF a b
arr x = \ y -> map x y

tag :: Event a -> b -> Event b
tag (Event a) y = Event y
tag NoEvent y =NoEvent

switch1 :: Eq c => Signal ( a , Event c) -> ( c -> Signal a) -> Signal a
switch1 x y = newfunc x y NoEvent

newfunc :: Eq c => Signal (a,Event c) -> (c -> Signal a) -> Event c -> Signal a
newfunc ((x1,x2):xs) y NoEvent = (x1: if x2 == NoEvent then newfunc xs y NoEvent else newfunc xs y x2)
newfunc xs y (Event c) = y c

(&&&) a b = \x -> zip (a x) (b x)


type Acceleration = Double
type Velocity     = Double
type Position     = Double
type Radius 	  = Double


data Ball         = Ball { ballRad  :: Radius
                         , ballPosX :: Position
                         , ballPosY :: Position
                         , ballVel  :: Velocity
                         } deriving Eq
g :: Acceleration
g = -1

ball1 :: Ball
ball1 = Ball { ballRad  = 0.04
             , ballPosX = 0.5
             , ballPosY = 0.8
             , ballVel  = - 0.0025
             }

tag1 :: Signal Ball -> Signal (Event a) -> Signal (Event Ball)
tag1 (b:bs) (e:es) = (tag e (b{ballVel= ballVel b * (-1)}) : tag1 bs es)

detectBounce :: SF Ball (Event Ball)
detectBounce balls =  tag1 balls (edge (map (\ x -> (ballPosY x <= ballRad x) || (ballPosY x >=0.8)) balls))
			 
				 
switchWhen :: Eq e => Signal a -> SF a (Event e) -> (e -> Signal a) -> Signal a
switchWhen sf sfe = switch1 ((identity &&& sfe) sf)

bouncingBall :: Ball -> Signal Ball
bouncingBall b = switchWhen (moving b) detectBounce bouncingBall

moving:: Ball -> Signal Ball
moving b = map (\x -> b {ballPosY = x}) (iIntegral (ballPosY b) (repeat (ballVel b)))




 
