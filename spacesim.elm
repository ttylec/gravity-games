module SpaceSim (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)

import Vectors exposing (..)

type alias Object = { renderer : Form
                    , size : Float
                    , color : Color
                    , orbit : Orbit
                    , position : Vector2D
                    , velocity : Vector2D
                    , orientation : Float}

type alias Orbit =
  { sma : Float     -- semi major axis
  , e : Float       -- eccentricity
  , omega : Float   -- argument of periapsis
  , meanL : Float   -- mean longitude at t=0
  }

--
-- Compute new state vector
--
rk4 : Float -> (Vector2D, Vector2D) -> (Vector2D -> Vector2D) -> (Vector2D, Vector2D)
rk4 h (r0, v0) a =
  let
    k1v = a r0
    k2v = a <| r0 .+. h/2 *. k1r
    k3v = a <| r0 .+. h/2 *. k2r
    k4v = a <| r0 .+. h *. k3r
    k1r = v0
    k2r = v0 .+. h/2 *. k1v
    k3r = v0 .+. h/2 *. k2v
    k4r = v0 .+. h *. k3v
  in
    (r0 .+. h/6 *. (k1r .+. 2 *. k2r .+. 2 *. k3r .+. k4r),
     v0 .+. (h/6) *. (k1v .+. 2 *. k2v .+. 2 *. k3v .+. k4v))


gravityG : Float -> Vector2D -> Vector2D
gravityG gm r =
  let
    r2 = r .*. r
    er = 1 / sqrt r2 *. r
  in
    (-1.0 * gm / r2) *. er

updateObject : Float -> Float -> Float -> Float -> Object -> Object
updateObject gm dt rotation thurst ship =
  let
    thurstV = thurst *. Vector2D (cos ship.orientation) (sin ship.orientation)
    force r = gravityG gm r .+. thurstV
    (r, v) = rk4 dt (ship.position, ship.velocity) force
  in
    {ship |
       position = r
    ,  velocity = v
    ,  orientation = ship.orientation + rotation * dt
    ,  orbit = if abs thurst > 1e-6 then orbitFromState gm r v else ship.orbit
    }


--
-- Orbits
--

-- Get orbital elements from the state vector
orbitFromState : Float -> Vector2D -> Vector2D -> Orbit
orbitFromState gm r v =
  let
    rr = sqrt (r .*. r)
    v2 = v .*. v
    h2 =  (exterior r v)^2  -- (xcomp r * ycomp v - ycomp x * xcomp v)^2
    energy = v2 / 2 - gm / rr
    sma = -1.0 * gm / (2 * energy)
    rv = r .*. v
    e = 1/gm *. ((v2 - gm/rr) *. r .+. -rv *. v)
    ecc = sqrt <| 1 + 2 * energy * h2 / gm^2
    er = e .*. r
    omega' = if ycomp e > 0 then acos (xcomp e / ecc)
             else 2 * pi - acos (xcomp e / ecc)
    omega = if isNaN omega' then 0 else omega'
    nu' = acos (er / (ecc * rr))
    nu = if rv < 0 then nu' else 2 * pi - nu'
    eaarg = (1 - rr / sma) / ecc
    eccentricAnomaly = if nu > pi then acos eaarg else 2 * pi - acos eaarg
    meanAnomaly = eccentricAnomaly - ecc * sin eccentricAnomaly
    meanL = meanAnomaly + omega
  in
    {sma = sma, e = ecc, omega = omega, meanL = meanL}

semiMinorAxis : Orbit -> Float
semiMinorAxis orbit = orbit.sma * sqrt (1 - orbit.e^2)

getVelocity : Float -> Orbit -> Vector2D
getVelocity gm elements =
  let
    (r, nu') = getHCPosition elements
    nu = nu' - elements.omega
    p = (semiMinorAxis elements)^2 / elements.sma
    h = sqrt (gm * p)
    vr = gm * elements.e * sin nu / h
    vtheta = h / r
  in
    Vector2D (vr * cos nu' - vtheta * sin nu')
               (vr * sin nu' + vtheta * cos nu')

getHCPosition : Orbit -> (Float, Float)
getHCPosition elements =
  let
    e = elements.e
    meanLongitude = elements.meanL  -- + t * meanMotion elements
    meanAnomaly = meanLongitude - elements.omega
    ea = eccentricAnomaly elements meanAnomaly
    tanlong2 = (sqrt <| (1 + e) / (1 - e)) * tan (ea / 2)
  in
    (elements.sma * (1 - e * cos ea)
    , elements.omega + 2 * atan tanlong2)

getPosition : Orbit -> Vector2D
getPosition elements =
  let
    (r, phi) = getHCPosition elements
  in
    Vector2D (r * cos phi) (r * sin phi)

eccentricAnomaly elements ma = eccentricAnomaly' elements ma ma 10
eccentricAnomaly' elements ma ea k =
  let
    ea' = ma + elements.e * sin ea
  in
    if k > 0 then eccentricAnomaly' elements ma ea' (k - 1)
    else ea


--
-- Drawing objects
--

drawObject : Bool -> Object -> Form
drawObject drawOrbits object =
  let
    xy = toTuple object.position
    obj = object.renderer |> rotate object.orientation |> move xy
  in
    if drawOrbits then group [obj, drawOrbit object.color object.orbit]
    else obj

drawOrbit : Color -> Orbit -> Form
drawOrbit color elements =
  let
    a = elements.sma
    b = semiMinorAxis elements
    c = a * elements.e
    orbitShape = oval (2 * a) (2 * b) |> outlined (dashed color)
    omega = elements.omega
  in
    orbitShape |> rotate omega |> move (-c * cos omega, -c * sin omega)

drawSun : Form
drawSun = oval 20 20 |> filled yellow


--
-- Object creation
--

objectFromElements : Float -> Form -> Float -> Orbit -> Object
objectFromElements gm renderer size orbit =
  let
    v = getVelocity gm orbit
  in
    { renderer = renderer
    , color = blue
    , size = size
    , orbit = orbit
    , position = getPosition orbit
    , velocity = v
    , orientation = atan2 (ycomp v) (xcomp v)
    }
