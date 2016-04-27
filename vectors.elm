module Vectors (..) where
-- module Vectors ( Vector2D(Vector2D), (.+.), (*.), (.*.)
--                , exterior, xcomp, ycomp, toTuple) where

type Vector2D = Vector2D Float Float

-- Vector calculus

(.+.) : Vector2D -> Vector2D -> Vector2D
(.+.) (Vector2D x1 x2) (Vector2D y1 y2) = Vector2D (x1 + y1) (x2 + y2)

infixl 4 .+.

(.-.) : Vector2D -> Vector2D -> Vector2D
(.-.) x y = x .+. (-1.0) *. y

(*.) : Float -> Vector2D -> Vector2D
(*.) a (Vector2D x y) = Vector2D (a * x) (a * y)

infixl 6 *.

(.*.) : Vector2D -> Vector2D -> Float
(.*.) (Vector2D x1 x2) (Vector2D y1 y2) = x1 * y1 + x2 * y2

infixl 8 .*.

exterior : Vector2D -> Vector2D -> Float
exterior (Vector2D x1 x2) (Vector2D y1 y2) = x1 * y2 - x2 * y1

toTuple : Vector2D -> (Float, Float)
toTuple (Vector2D x y) = (x, y)

xcomp : Vector2D -> Float
xcomp (Vector2D x _) = x

ycomp : Vector2D -> Float
ycomp (Vector2D _ y) = y

norm : Vector2D -> Float
norm x = sqrt <| x .*. x

polar2Cartesian : (Float, Float) -> Vector2D
polar2Cartesian (r, phi) = Vector2D (r * cos phi) (r * sin phi)
