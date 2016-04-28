module Mars (main) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Window
import Time exposing (..)
import Keyboard
import List exposing (..)
import Char
-- import Debug

import SpaceSim exposing (..)
import Vectors exposing (..)

type alias System = { planets : List Object
                    , spaceship : Object
                    , drawOrbits : Bool
                    }

type Input = Simulation Bool Int Int Time | ToggleOrbits Bool

-- Game constants
rotationSpeed = -1 * degrees 360
accel = 1

earthT = 36
au = 150
gm = 4 * pi^2 * au^3 / earthT^2

-- Orbiting bodies
elementsEarth =
  { sma = au
  , e = 0.017
  , omega =  degrees 103
  , meanL = degrees 100
  }

elementsMars =
  { sma = 1.52 * au
  , e = 0.093
  , omega =  degrees 336
  , meanL = degrees 355
  }

elementsVenus =
    { sma = 0.72 * au
    , e = 0.007
    , omega =  degrees 131
    , meanL = degrees 182
  }

elementsComet =
    { sma = 1.7 * au
    , e = 0.9
    , omega =  degrees 0
    , meanL = degrees 0
    }

earth = { renderer = oval 10 10 |> filled green
        , size = 5
        , orbit = elementsEarth
        , color = blue
        , position = getPosition elementsEarth
        , velocity = getVelocity gm elementsEarth
        , orientation = 0
        }

mars = { renderer = oval 5 5 |> filled red
       , size = 2
       , orbit = elementsMars
       , color = blue
       , position = getPosition elementsMars
       , velocity = getVelocity gm elementsMars
       , orientation = 0
       }

venus = { renderer = oval 5 5 |> filled orange
       , size = 5
       , orbit = elementsVenus
       , color = blue
       , position = getPosition elementsVenus
       , velocity = getVelocity gm elementsVenus
       , orientation = 0
       }


comet = { renderer = oval 3 3 |> filled white
        , size = 2
        , orbit = elementsComet
        , color = blue
        , position = getPosition elementsComet
        , velocity = getVelocity gm elementsComet
        , orientation = 0
        }


spaceship = { renderer = shipRenderer
            , size = 7.5
            , orbit = elementsEarth
            , color = green
            , position = getPosition elementsEarth
            , velocity = getVelocity gm elementsEarth
            , orientation = pi / 4}

system = { planets = [earth, mars, venus, comet]
         , spaceship = spaceship
         , drawOrbits = False
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

gameWidth = 800
gameHeight = 600

view : (Int, Int) -> System -> Element
view (w, h) system =
  let
    static = [ rect gameWidth gameHeight |> filled black , drawObject False sun ]
    planets = map (drawObject system.drawOrbits) system.planets
    spaceship = [drawObject system.drawOrbits system.spaceship]
  in
    container w h middle <|
              collage gameWidth gameHeight <| concat [static, planets, spaceship]


update : Input -> System -> System
update input system =
  case input of
    Simulation space dir burn delta ->
      let
        dt = if space then 0.1 * delta else delta
        thurst = if space then 10 * accel * (toFloat burn)
                 else accel * (toFloat burn)
        rot = if space then -10 * rotationSpeed * (toFloat dir)
              else rotationSpeed * (toFloat dir)
        ship = updateObject gm dt rot thurst system.spaceship
      in
        { system |
            planets = map (updateObject gm dt 0 0) system.planets
        ,   spaceship = {ship | renderer = if burn > 0 then shipPRenderer
                                           else shipRenderer}
        }

    ToggleOrbits toggle ->
      { system |
          drawOrbits = if toggle then not system.drawOrbits
                       else system.drawOrbits
      }


input : Signal Input
input = Signal.merge (Signal.map ToggleOrbits <| Keyboard.isDown 79) simInput

delta = Signal.map inSeconds (fps 35)

simInput : Signal Input
simInput = Signal.sampleOn delta <|
              Signal.map4 Simulation
                   Keyboard.space
                   (Signal.map .x Keyboard.arrows)
                   (Signal.map .y Keyboard.arrows)
                   delta

simulationState : Signal System
simulationState = Signal.foldp update system input

main = Signal.map2 view Window.dimensions simulationState
