module Cube where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Array (mapWithIndex, filter, (!!), drop, cons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (cos, sin)
import Svg.Attributes as SA
import Svg.Elements as SE
import DOM.Event.KeyboardEvent (code)

-- Core Types
type Distance = Number

type Angle = Number

type Point2D =
  { x :: Distance
  , y :: Distance
  }

type Point3D =
  { x :: Distance
  , y :: Distance
  , z :: Distance
  , i :: Int
  }

type Edge = Tuple Int Int

type Shape =
  { vertices :: Array Point3D
  , colors :: Array SA.Color
  , surfaces :: Array Surface
  , normals :: Array Point3D
  }

type Angle3D =
  { xa :: Angle
  , ya :: Angle
  , za :: Angle
  }

type AngVelocity3D = Angle3D -- velocity = angle/sec

type RotatingShape =
  { shape :: Shape
  , angVel :: AngVelocity3D
  , angInc :: Number
  , angDir :: Number
  , size :: Number
  }

type Surface = Array Int

data Axis = X | Y | Z
data Keys = KeyX | KeyY | KeyZ | KeyPlus | KeyRemove

-- Model / State
type State = 
  {
    cubes :: Array RotatingShape
  }

-- Values

viewBoxSize :: Number
viewBoxSize = 600.0

viewCenter :: Point2D
viewCenter =
  { x: viewBoxSize / 2.0
  , y: viewBoxSize / 2.0
  }

frameRate :: Number
frameRate = 200.0

oneDegInRad :: Angle
oneDegInRad = 0.01745329255

tenDegInRad :: Angle
tenDegInRad = oneDegInRad * 10.0

accelerateBy :: Number
accelerateBy = oneDegInRad * 50.0

dampenPercent :: Number
dampenPercent = 1.0 - (0.9 / frameRate) -- 10% per second

initCube :: State
initCube =
  {
    cubes: [
      { shape:
          { vertices:
              [ { x:  100.0, y:  100.0, z:  100.0, i: 0 }
              , { x: -100.0, y:  100.0, z:  100.0, i: 0 }
              , { x:  100.0, y: -100.0, z:  100.0, i: 0 }
              , { x: -100.0, y: -100.0, z:  100.0, i: 0 }
              , { x:  100.0, y:  100.0, z: -100.0, i: 0 }
              , { x: -100.0, y:  100.0, z: -100.0, i: 0 }
              , { x:  100.0, y: -100.0, z: -100.0, i: 0 }
              , { x: -100.0, y: -100.0, z: -100.0, i: 0 }
              ]
          , surfaces: 
              [ [1, 0, 2, 3]
              , [0, 2, 6, 4]
              , [0, 1, 5, 4]
              , [4, 5, 7, 6]
              , [1, 3, 7, 5]
              , [2, 3, 7, 6]
              ]
           , colors:
              [ (SA.RGB 100 100 0)
              , (SA.RGB 0 150 75)
              , (SA.RGB 255 0 150)
              , (SA.RGB 255 255 0)
              , (SA.RGB 0 150 150)
              , (SA.RGB 255 0 0)
              ]
          , normals:
              [ { x: 0.0,    y: 0.0,    z: 100.0,  i: 0}
              , { x: 100.0,  y: 0.0,    z: 0.0,    i: 1}
              , { x: 0.0,    y: 100.0,  z: 0.0,    i: 2}
              , { x: -100.0, y: 0.0,    z: 0.0,    i: 3}
              , { x: 0.0,    y: 0.0,    z: -100.0, i: 4}
              , { x: 0.0,    y: -100.0, z: 0.0,    i: 5}
              ]
          }
          , angVel:
              { xa: tenDegInRad
              , ya: tenDegInRad
              , za: tenDegInRad
              }
          , angInc: 0.0
          , angDir: 1.0
          , size: 10.0
      }
    ]
  }

-- Events
data Query a
  = Tick a
  | IncAngSpeed Axis a
  | IncreaseSpeed Int a
  | DecreaseSpeed Int a
  | AddCube a
  | RemoveCube a
  | ReverseDir Int a
  | AdjustSize String a
  | NoAction a
  
-------------------- VIEW --------------------
renderView :: State -> H.ComponentHTML Query
renderView state = let
    { cubes } = state
  in
    HH.div 
      [ HP.id_ "div1"
      , HE.onKeyDown (\e -> case code e of
              "Numpad1" -> Just (H.action (IncAngSpeed X))
              "Numpad2" -> Just (H.action (IncAngSpeed Y))
              "Numpad3" -> Just (H.action (IncAngSpeed Z))
              "KeyA" -> Just (H.action AddCube)
              "KeyR" -> Just (H.action RemoveCube)
              _     -> Just (H.action NoAction))
      ] $
      [ HH.br_
      , HH.div [] $
        [
          renderButton "RotateX" (IncAngSpeed X)
        , renderButton "RotateY" (IncAngSpeed Y)
        , renderButton "RotateZ" (IncAngSpeed Z)
        , renderButton "Add Cube" (AddCube)
        , renderButton "Remove Cube" (RemoveCube)
        , renderGuide "Adjut Cube Size"
        , renderSlider
        ]
      ]
      <>
      [ renderGuide "Click RotateX/RotateY/RotateZ buttons or 1/2/3 key to rotate along X-axis/Y-axis/Z-axis" 
      , renderGuide "Click A / R key to add and remove cube"
      ]
      <>
      mapWithIndex renderCube cubes
  where
    renderButton label query =
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ query)
        ]
        [ HH.text label ]
    
    renderGuide label = 
      HH.p_
        [ HH.text label
        ]
    renderSlider =
      HH.input  [ HP.type_ HP.InputRange
                , HP.min 5.0
                , HP.max 10.0
                , HP.required true
                , HE.onValueInput $ HE.input AdjustSize
                ]

renderCube :: Int -> RotatingShape -> H.ComponentHTML Query
renderCube idx cube = let
    {vertices, normals, surfaces, colors} = cube.shape
    normals' = filter (\normal -> normal.z > 0.0) normals
    surfaces' = map (\normal -> fromMaybe [] (surfaces !! normal.i)) normals'
    colors' = map (\normal -> getColor (colors !! normal.i)) normals'
    vert2Ds = map (project cube.size) vertices
  in
      HH.div [] $ 
        [ 
          renderButton "Reverse Direction" (ReverseDir idx)
        , renderButton "Increase Speed" (IncreaseSpeed idx)
        , renderButton "Decrease Speed" (DecreaseSpeed idx)
        ]
        <>
        [ SE.svg
          [ SA.viewBox 0.0 0.0 viewBoxSize viewBoxSize ]
          [ SE.g []
            (drawCube surfaces' vert2Ds colors')
          ]
        ]
  where
    renderButton label query =
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ query)
        ]
        [ HH.text label ]

    -- parallel projection
    project :: Number -> Point3D -> Point2D
    project size p =
      { x: p.x*size*0.1 + viewCenter.x
      , y: p.y*size*0.1 + viewCenter.y
      }

    drawCube :: Array Surface -> Array Point2D -> Array SA.Color -> Array (H.ComponentHTML Query)
    drawCube surfaces vert2Ds colors = 
      drawSurfaces surfaces vert2Ds colors <> drawVertices vert2Ds

    drawSurfaces :: Array Surface -> Array Point2D -> Array SA.Color -> Array (H.ComponentHTML Query)
    drawSurfaces surfaces verts colors = 
        mapWithIndex (\idx surface -> 
          drawRect (getPoint (verts !! getInt (surface !! 0))) (getPoint (verts !! getInt (surface !! 1))) (getPoint (verts !! getInt (surface !! 2))) (getPoint (verts !! getInt (surface !! 3))) (getColor (colors !! idx))) surfaces

    getInt :: Maybe Int -> Int
    getInt mi =
      fromMaybe 0 mi
    
    getColor :: Maybe SA.Color -> SA.Color
    getColor mc = let
        default = (SA.RGB 0 0 0)
      in
        fromMaybe default mc

    getPoint :: Maybe Point2D -> Point2D
    getPoint maybePoint = let
       default = { x: 100.0, y: 100.0 }
      in
        fromMaybe default maybePoint

    drawRect :: Point2D -> Point2D -> Point2D -> Point2D -> SA.Color -> H.ComponentHTML Query
    drawRect a b c d color =
      SE.path
        [ SA.d
          [ SA.Abs (SA.M a.x a.y)
          , SA.Abs (SA.L b.x b.y)
          , SA.Abs (SA.L c.x c.y)
          , SA.Abs (SA.L d.x d.y)
          ]
        , SA.stroke $ Just (SA.RGB 150 0 0)
        , SA.fill $ Just color
        ]

    -- drawRect :: Point2D -> Point2D -> Point2D -> Point2D -> SA.Color -> H.ComponentHTML Query
    -- drawRect a b c d color =
    --   SE.path
    --     [ SA.d
    --       [ SA.Abs (SA.L a.x a.y)
    --       , SA.Abs (SA.L b.x b.y)
    --       , SA.Abs (SA.L c.x c.y)
    --       , SA.Abs (SA.L d.x d.y)
    --       ]
    --     , SA.stroke $ Just (SA.RGB 150 0 0)
    --     , SA.fill $ Just color
    --     ]
        

    drawVertices :: Array Point2D -> Array (H.ComponentHTML Query)
    drawVertices vert2Ds =
      mapWithIndex drawVertex vert2Ds

    drawVertex :: Int -> Point2D -> H.ComponentHTML Query
    drawVertex idx {x, y} = SE.g []
      [ SE.text
          [ SA.x $ x + 5.0
          , SA.y $ y - 5.0
          , SA.fill $ Just (SA.RGB 0 0 0)
          ]
          [ HH.text $ show idx ]
      , SE.circle
          [ SA.r 3.0
          , SA.cx x
          , SA.cy y
          , SA.fill $ Just (SA.RGB 0 0 0)
          ]
      ]

-------------------- UPDATE / REDUCERS --------------------
cubes :: forall eff. H.Component HH.HTML Query Unit Unit (Aff (console :: CONSOLE | eff))
cubes = do
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    initialState :: State
    initialState = initCube

    render :: State -> H.ComponentHTML Query
    render = renderView

    eval :: Query ~> H.ComponentDSL State Query Unit (Aff (console :: CONSOLE | eff))
    eval = case _ of
      Tick next -> do
        cubeState <- H.get
        let { cubes } = cubeState
        let newCubes = map (\cube ->
              cube { 
                shape { 
                    vertices = rotateShape cube.shape.vertices (anglePerFrame cube.angVel)
                  , normals = rotateShape cube.shape.normals (anglePerFrame cube.angVel)
                  }
                , angVel = dampenAngSpeed cube.angVel 
                , angInc = cube.angInc
                , angDir = cube.angDir
                , size = cube.size }
            ) cubes
        H.put cubeState { cubes = newCubes }
        pure next

      IncAngSpeed axis next -> do
        H.liftEff (log "rotate")
        cubeState <- H.get
        let { cubes } = cubeState
        let newCubes = map (\cube ->
                              case axis of
                                X -> cube { angVel { xa = cube.angDir * (cube.angVel.xa + cube.angInc) } }
                                Y -> cube { angVel { ya = cube.angDir * (cube.angVel.ya + cube.angInc) } }
                                Z -> cube { angVel { za = cube.angDir * (cube.angVel.za + cube.angInc) } }
                            ) cubes
        H.put cubeState { cubes = newCubes }
        pure next

      AddCube next -> do -- Create New Cube
        H.liftEff $ log "add_cube"
        cubeState <- H.get
        let { cubes } = cubeState
        let newCubes = cons {
                        shape:
                            { vertices:
                                [ { x:  100.0, y:  100.0, z:  100.0, i: 0 }
                                , { x: -100.0, y:  100.0, z:  100.0, i: 0 }
                                , { x:  100.0, y: -100.0, z:  100.0, i: 0 }
                                , { x: -100.0, y: -100.0, z:  100.0, i: 0 }
                                , { x:  100.0, y:  100.0, z: -100.0, i: 0 }
                                , { x: -100.0, y:  100.0, z: -100.0, i: 0 }
                                , { x:  100.0, y: -100.0, z: -100.0, i: 0 }
                                , { x: -100.0, y: -100.0, z: -100.0, i: 0 }
                                ]
                            , surfaces: 
                                [ [1, 0, 2, 3]
                                , [0, 2, 6, 4]
                                , [0, 1, 5, 4]
                                , [1, 3, 7, 5]
                                , [4, 5, 7, 6]
                                , [2, 3, 7, 6]
                                ]
                            , colors:
                                [ (SA.RGB 100 100 0)
                                , (SA.RGB 0 150 75)
                                , (SA.RGB 255 0 150)
                                , (SA.RGB 255 255 0)
                                , (SA.RGB 0 150 150)
                                , (SA.RGB 255 0 0)
                                ]
                            , normals:
                                [ { x: 0.0,    y: 0.0,    z: 100.0,  i: 0}
                                , { x: 100.0,  y: 0.0,    z: 0.0,    i: 1}
                                , { x: 0.0,    y: 100.0,  z: 0.0,    i: 2}
                                , { x: -100.0, y: 0.0,    z: 0.0,    i: 3}
                                , { x: 0.0,    y: 0.0,    z: -100.0, i: 4}
                                , { x: 0.0,    y: -100.0, z: 0.0,    i: 5}
                                ]
                            }
                        , angVel:
                            { xa: tenDegInRad
                            , ya: tenDegInRad
                            , za: tenDegInRad
                            }
                        , angInc: 1.0
                        , angDir: 1.0
                        , size: 10.0
                      } cubes
        H.put cubeState { cubes = newCubes }
        pure next

      RemoveCube next -> do -- Remove Cube
        H.liftEff $ log "remove_cube"
        cubeState <- H.get
        let { cubes } = cubeState
        let newCubes = drop 1 cubes
        H.put cubeState { cubes = newCubes }
        pure next

      ReverseDir idx next -> do
        H.liftEff $ log "reverse_direction"
        cubeState <- H.get
        let { cubes } = cubeState
        let newCubes = mapWithIndex (changeDir idx) cubes
        H.put cubeState { cubes = newCubes }
        pure next
      
      AdjustSize sizeStr next -> do
        H.liftEff $ log "adjust_size"
        cubeState <- H.get
        let { cubes } = cubeState
        let newCubes = map  (\cube ->
                              adjustSize (fromMaybe 0.0 (fromString sizeStr)) cube
                            ) cubes
        H.put cubeState { cubes = newCubes }
        pure next
        
      NoAction next -> do
        H.liftEff $ log "no action"
        pure next

      IncreaseSpeed idx next -> do
        H.liftEff $ log "increase_speed"
        cubeState <- H.get
        let { cubes } = cubeState
        let newCubes = mapWithIndex (increaseSpeed idx) cubes
        H.put cubeState { cubes = newCubes }
        pure next

      DecreaseSpeed idx next -> do
        H.liftEff $ log "decrease_speed"
        cubeState <- H.get
        let { cubes } = cubeState
        let newCubes = mapWithIndex (decreaseSpeed idx) cubes
        let newCubeState = cubeState { cubes = newCubes }
        H.put newCubeState
        pure next

      -- AdjustSize sizeStr next -> do
      --   H.liftEff $ log "change_size"
      --   cubeState <- H.get
      --   let { cubes } = cubeState
      --   let newCubes = map  (\cube ->
      --                         adjustSize (fromMaybe 0.0 (fromString sizeStr)) cube
      --                       ) cubes
      --   H.put cubeState { cubes = newCubes }
      --   pure next

rotateShape :: Array Point3D -> AngVelocity3D -> Array Point3D
rotateShape vertices ang =
  map (rotate ang) vertices

rotate :: AngVelocity3D -> Point3D -> Point3D
rotate { xa, ya, za } = rotateX xa >>> rotateY ya >>> rotateZ za
  where
    rotateX ang {x,y,z,i} = let Tuple ny nz = rotateInPlane y z ang in { x, y:ny, z:nz, i }
    rotateY ang {x,y,z,i} = let Tuple nx nz = rotateInPlane x z ang in { x:nx, y, z:nz, i }
    rotateZ ang {x,y,z,i} = let Tuple nx ny = rotateInPlane x y ang in { x:nx, y:ny, z, i }

    rotateInPlane :: Number -> Number -> Number -> Tuple Number Number
    rotateInPlane axis1 axis2 ang =
      Tuple (axis1 * cos(ang) - axis2 * sin(ang)) (axis2 * cos(ang) + axis1 * sin(ang))

anglePerFrame :: AngVelocity3D -> Angle3D
anglePerFrame {xa, ya, za} =
  { xa: xa / frameRate
  , ya: ya / frameRate
  , za: za / frameRate
  }

dampenAngSpeed :: AngVelocity3D -> AngVelocity3D
dampenAngSpeed {xa, ya, za} =
    { xa: dampen xa
    , ya: dampen ya
    , za: dampen za
    }
  where
    dampen :: Number -> Number
    dampen ang = ang * dampenPercent -- Basics.max 0 (ang-drpf)

changeDir :: Int -> Int -> RotatingShape -> RotatingShape
changeDir id idx cube = 
  if id == idx
  then 
    {
      shape: cube.shape
    , angVel: cube.angVel
    , angInc: cube.angInc
    , angDir: cube.angDir * -1.0
    , size: cube.size
    }
  else 
    cube


adjustSize :: Number -> RotatingShape -> RotatingShape
adjustSize size cube = 
    {
      shape: cube.shape
    , angVel: cube.angVel
    , angInc: cube.angInc
    , angDir: cube.angDir * -1.0
    , size: size
    }

increaseSpeed :: Int -> Int -> RotatingShape -> RotatingShape
increaseSpeed id idx cube =
  if id == idx
  then 
    {
      shape: cube.shape
    , angVel: cube.angVel
    , angInc: cube.angInc + 2.0
    , angDir: cube.angDir
    , size: cube.size
    }
  else 
    cube

-- increaseSpeed :: Int -> Int -> RotatingShape -> RotatingShape
-- increaseSpeed id idx cube =
--   if id == idx
--   then 
--     {
--       shape: cube.shape
--     , angVel: cube.angVel + 2.0
--     , angInc: cube.angInc + 2.0
--     , angDir: cube.angDir
--     , size: cube.size
--     }
--   else 
--     cube

decreaseSpeed :: Int -> Int -> RotatingShape -> RotatingShape
decreaseSpeed id idx cube =
  if id == idx
  then 
    {
      shape : cube.shape
    , angVel: cube.angVel
    , angInc: cube.angInc - 2.0
    , angDir: cube.angDir
    , size: cube.size
    }
  else 
    cube

