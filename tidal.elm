module Tidal (main) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Time exposing (..)
import Keyboard
import List exposing (..)
import Char
import Text
import Random exposing (initialSeed, float, generate, pair, Generator, Seed)
import Drag
import Debug

import SpaceSim exposing (..)
import Vectors exposing (..)

type alias Game = { particles : List Object
                  , clickXY : Vector2D }

type Input = Insert Drag.MouseEvent | Simulation Time


gameWidth = 800
gameHeight = 600

startT = 40
au = 400
gm = 4 * pi^2 * au^3 / startT^2

elementsDefault =
  { sma = 0.3 * au
  , e = 0.0
  , omega = 0
  , meanL = 0
  }

sun = { renderer = sunRenderer
      , size = 20
      , orbit = { sma = 0, e = 0, omega = 0, meanL = 0}
      , color = yellow
      , position = Vector2D 0 0
      , velocity = Vector2D 0 0
      , orientation = 0
      }

particle : Vector2D -> Vector2D -> Object
particle r v = { renderer = circle 1 |> filled white
               , size = 1
               , orbit = { sma = 0, e = 0, omega = 0, meanL = 0}
               , color = yellow
               , position = r
               , velocity = v
               , orientation = 0
               }

pairs : List a -> List b -> List (a, b)
pairs la lb = concat <| map (pairs' la) lb
pairs' la b = map (\a -> (a, b)) la

randomList : Generator a -> Seed -> Int -> List a
randomList gen s n =
  let
    (x, s') = generate gen s
    rest = if n > 1 then randomList gen s' (n-1)
           else []
  in
    x :: rest

planet : Vector2D -> Vector2D -> List Object
planet r v =
  let
    seed = initialSeed 0
    gen = pair (float 0 10) (float 0 (2*pi))
    dr = map polar2Cartesian <| randomList gen seed 200
  in
    map (\r' -> particle (r .+. r') v) dr

game = { particles = planet (getPosition elementsDefault)
                     (getVelocity gm elementsDefault)
       , clickXY = Vector2D 0 0
       }

sunRenderer = fittedImage 40 40 "sun.gif" |> toForm

view : (Int, Int) -> Game -> Element
view (w, h) {particles, clickXY} =
  let
    static = [ rect gameWidth gameHeight |> filled black , drawObject False sun ]
    ps = map (drawObject False) particles
  in
    container w h topLeft <|
              collage gameWidth gameHeight <|
                      concat [static, ps]

inGameField : Object -> Bool
inGameField object =
  let
    r = sqrt <| object.position .*. object.position
    x = xcomp <| object.position
    y = ycomp <| object.position
  in
    r > 20 && x > -gameWidth/2 && x < gameWidth/2 &&
      y > -gameHeight/2 && y < gameHeight/2

update : Input -> Game -> Game
update input game =
       case input of
         Simulation dt ->
           let
             particles' = filter inGameField <| map (updateObject gm dt 0 0)
                          game.particles
           in
             {game | particles = particles'}
         Insert event -> insertEvent event game

mouseToVector : (Int, Int) -> Vector2D
mouseToVector (x, y) =
  let
    x' = (toFloat x) - gameWidth/2
    y' = gameHeight/2 - (toFloat y)
  in
    Vector2D x' y'

insertEvent : Drag.MouseEvent -> Game -> Game
insertEvent event game =
  case event of
    Drag.StartAt (x, y) -> {game | clickXY = mouseToVector (x, y)}
    Drag.EndAt (x, y) ->
      let
        xx = Debug.watch "end xy" <| (x, y)
        end = mouseToVector (x, y)
        r = game.clickXY
        v = end .-. game.clickXY
      in
        {game | particles = append (planet r v) game.particles }
    _ -> game

delta = Signal.map inSeconds (fps 35)

input : Signal Input
input =
  Signal.merge (Signal.map Insert Drag.mouseEvents) inputSim

inputSim : Signal Input
inputSim = Signal.sampleOn delta <| Signal.map Simulation delta

gameState : Signal Game
gameState = Signal.foldp update game input

main = Signal.map2 view Window.dimensions gameState
