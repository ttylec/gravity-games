module Shooter (main) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Time exposing (..)
import Keyboard
import List exposing (..)
import Char
import Text
import Debug

import SpaceSim exposing (..)
import Vectors exposing (..)

type alias Ship = { object : Object
                  , destroyed : Bool
                  , energy : Float
                  , fuel : Float
                  }

type alias Game = { ship1 : Ship
                  , ship2 : Ship
                  , score1 : Int
                  , score2 : Int
                  , bullets : List Object
                  , drawOrbits : Bool
                  }

type alias Action = {x: Int, y: Int}

type Input = Simulation Action Action Time
           | Shoot Bool Bool
           | ToggleOrbits Bool
           | Restart Bool

gameWidth = 800
gameHeight = 600


-- Game constants
rotationSpeed = -1.0 * degrees 180
accel = 100

bulletV = 100
bulletEnergy = 2
energyGain = 1

startT = 40
au = 400
gm = 4 * pi^2 * au^3 / startT^2

elementsPlayer1 =
  { sma = 0.5 * au
  , e = 0.0
  , omega = degrees 0
  , meanL = degrees 10
  }

elementsPlayer2 =
  { sma = 0.5 * au
  , e = 0.0
  , omega = degrees 0
  , meanL = degrees 190
  }

sun = { renderer = sunRenderer
      , size = 20
      , orbit = { sma = 0, e = 0, omega = 0, meanL = 0}
      , color = yellow
      , position = Vector2D 0 0
      , velocity = Vector2D 0 0
      , orientation = 0
      }
sunRenderer = fittedImage 40 40 "sun.gif" |> toForm
shipRenderer = fittedImage 15 15 "spaceship.gif" |> toForm
shipPRenderer = fittedImage 20 20 "spaceship-propelled.gif" |> toForm
destroyedRenderer = fittedImage 15 15 "spaceship-destroyed.gif" |> toForm
bulletRenderer = oval 2 2 |> filled white

startShip1 = { object = objectFromElements gm shipRenderer 7.5 elementsPlayer1
             , destroyed = False
             , energy = 10
             , fuel = 100}

startShip2 = { object = objectFromElements gm shipRenderer 7.5 elementsPlayer2
             , destroyed = False
             , energy = 10
             , fuel = 100}

game = { ship1 = startShip1 , ship2 = startShip2
       , score1 = 0 , score2 = 0
       , bullets = []
       , drawOrbits = False
       }

viewEnergy : Ship -> Form
viewEnergy ship =
  let
    ls = {defaultLine | color = hudGreen, width = 10}
  in
    traced ls <| path [(0, 0), (0, 10 * ship.energy)]

hudGreen = rgb 160 200 160

viewScore : Int -> Form
viewScore s =
  Text.fromString (toString s) |> Text.color hudGreen
                               |> Text.monospace
                               |> Text.height 20
                               |> leftAligned
                               |> toForm

drawShip : Bool -> Ship -> Form
drawShip do ship =
  let
    object = ship.object
  in if ship.destroyed then
       drawObject do {object | renderer = destroyedRenderer}
     else
       drawObject do object

view : (Int, Int) -> Game -> Element
view (w, h) game =
  let
    static = [ rect gameWidth gameHeight |> filled black , drawObject False sun ]
    bullets = map (drawObject game.drawOrbits) game.bullets
    ship1 = [drawShip game.drawOrbits game.ship1]
    ship2 = [drawShip game.drawOrbits game.ship2]
    energies = [ viewEnergy game.ship2 |> move (-gameWidth/2 + 10, -gameHeight/2 + 10)
               , viewEnergy game.ship1 |> move (gameWidth/2 - 10, -gameHeight/2 + 10)]
    scores = [ viewScore game.score2 |> move (-gameWidth/2 + 10, -gameHeight/2 + 130)
             , viewScore game.score1 |> move (gameWidth/2 - 10, -gameHeight/2 + 130)]
  in
    container w h middle <|
              collage gameWidth gameHeight <|
                      concat [static, bullets, ship1, ship2,
                              energies, scores]

shoot : Ship -> Object
shoot ship =
  let
    object = ship.object
    dx = Vector2D (object.size * cos object.orientation)
                  (object.size * sin object.orientation)
  in
    {object |
       renderer = bulletRenderer
    ,  size = 0
    ,  color = white
    ,  position = object.position .+. dx
    }

updateShip : (Int, Int) -> Float -> Ship -> Ship
updateShip (dir, burn) dt ship =
  let
    thurst = if burn > 0 then accel * (toFloat burn)
             else 0
    energy = ship.energy + dt * energyGain
    rot = rotationSpeed * (toFloat dir)
    object = updateObject gm dt rot thurst ship.object
  in
    { ship |
        object = {object | renderer = if burn > 0 then shipPRenderer
                                      else shipRenderer}
    ,   energy = if energy >= 10 then 10 else energy
    }

inGameField : Object -> Bool
inGameField object =
  let
    r = sqrt <| object.position .*. object.position
    x = xcomp <| object.position
    y = ycomp <| object.position
  in
    r > 20 && x > -gameWidth/2 && x < gameWidth/2 &&
      y > -gameHeight/2 && y < gameHeight/2

isHit: Object -> List Object -> Bool
isHit ship objects =
  let
    hits o = norm (ship.position .-. o.position) <= ship.size + o.size
  in
    any hits objects

updateSimulation : Action -> Action -> Float -> Game -> Game
updateSimulation player1 player2 dt game =
  let
    ship1 = updateShip (player1.x, player1.y) dt game.ship1
    ship2 = updateShip (player2.x, player2.y) dt game.ship2
    bullets = filter inGameField <| map (updateObject gm dt 0 0) game.bullets
    hit1 = not (inGameField ship1.object)
           || isHit ship1.object (ship2.object :: bullets)
    hit2 = not (inGameField ship2.object)
           || isHit ship2.object (ship1.object :: bullets)
  in
    { game |
        ship1 = { ship1 | destroyed = hit1 || ship1.destroyed }
    ,   ship2 = { ship2 | destroyed = hit2 || ship2.destroyed }
    ,   score1 = if hit2 then game.score1 + 1 else game.score1
    ,   score2 = if hit1 then game.score2 + 1 else game.score2
    ,   bullets = bullets
    }

shootBullet : Ship -> (Ship, List Object)
shootBullet ship =
  let
    object = ship.object
    dx = Vector2D (object.size * cos object.orientation)
                  (object.size * sin object.orientation)
    dv = Vector2D (bulletV * cos object.orientation)
                  (bulletV * sin object.orientation)
    canShoot = ship.energy >= bulletEnergy
    bullet = if canShoot
             then [{object |
                      renderer = bulletRenderer
                   ,  size = 0
                   ,  color = white
                   ,  position = object.position .+. dx
                   ,  velocity = object.velocity .+. dv
                   }]
             else []
    energy = if canShoot then ship.energy - bulletEnergy
             else ship.energy
  in
    ({ship | energy = energy}, bullet)

update : Input -> Game -> Game
update input game =
  let
    destroyed = game.ship1.destroyed || game.ship2.destroyed
  in
    case input of
      Simulation player1 player2 delta ->
        if destroyed then
          game
        else
          updateSimulation player1 player2 delta game
      Shoot player1 player2 ->
        let
          t = Debug.watch "p1" <| player1
          u = Debug.watch "p2" <| player2
          (ship1, b1) = if player1 then shootBullet game.ship1
                        else (game.ship1, [])
          (ship2, b2) = if player2 then shootBullet game.ship2
                        else (game.ship2, [])
        in
          { game |
              bullets = concat [b1, b2, game.bullets]
          ,   ship1 = ship1
          ,   ship2 = ship2
          }
      ToggleOrbits toggle ->
        { game |
            drawOrbits = if toggle then not game.drawOrbits
                        else game.drawOrbits
        }
      Restart toggle ->
        if toggle && destroyed then
          { game |
              ship1 = startShip1
          ,   ship2 = startShip2
          ,   bullets = []
          }
        else
          game

input : Signal Input
input = Signal.mergeMany [ Signal.map ToggleOrbits <| Keyboard.isDown 79
                         , Signal.map Restart <| Keyboard.isDown 32
                         , shotInput
                         , simInput ]

delta = Signal.map inSeconds (fps 35)

shotInput : Signal Input
shotInput =
  Signal.merge
          (Signal.map (\x -> Shoot x False) (Keyboard.isDown 40))
          (Signal.map (\x -> Shoot False x) (Keyboard.isDown 83))


simInput : Signal Input
simInput = Signal.sampleOn delta <|
           Signal.map3 Simulation
                   Keyboard.arrows
                   Keyboard.wasd
                   delta


gameState : Signal Game
gameState = Signal.foldp update game input

main = Signal.map2 view Window.dimensions gameState
