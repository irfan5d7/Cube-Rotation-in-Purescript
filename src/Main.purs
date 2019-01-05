module Main where
import Data.Array
import Data.Maybe
import Graphics.Canvas
import Math
import Partial.Unsafe
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref

type Event = { clientX :: Number, clientY :: Number}

foreign import addEventListener :: forall a. CanvasElement -> String -> (Event -> Effect Unit) -> Effect Unit
foreign import set :: String -> Number -> Effect Unit
foreign import get :: String ->Effect  Number

type Vertex = {x :: Number, y :: Number, z :: Number}

initialVertices :: Array Vertex
initialVertices = [ {x : -100.0,y: -100.0,z: -100.0} 
                  , {x: -100.0,y: -100.0,z:100.0}
                  , {x: -100.0,y:100.0,z: -100.0}
                  , {x: -100.0,y:100.0,z:100.0}
                  , {x:100.0,y: -100.0,z: -100.0}
                  , {x:100.0,y: -100.0,z:100.0}
                  , {x:100.0,y:100.0,z: -100.0}
                  , {x:100.0,y:100.0,z:100.0}
                  ]
main :: Effect Unit
main = void $ unsafePartial do
    
    Just canvas <- getCanvasElementById "canvas"
    ctx <- getContext2D canvas
    translate ctx {translateX: 200.0, translateY : 200.0}
    vertex <- pure $ rotateX 60.0 initialVertices
    vertex <- pure $ rotateY 60.0 vertex
    project vertex ctx 
    ref <- new vertex
    addEventListener canvas "mousedown" onMouseDown 
    addEventListener canvas "mousemove" (onMouseMove ref ctx) 
    pure unit

onMouseDown :: Event -> Effect Unit
onMouseDown event = do
  set "x" event.clientX
  set "y" event.clientY

onMouseMove :: Ref (Array Vertex)-> Context2D -> Event -> Effect Unit
onMouseMove ref ctx event = do
  oldX <- get "x"
  oldY <- get "y"
  vertex <- read ref
  vertex <- pure $ rotateX ((event.clientY - oldY)/ 1000.0) vertex
  vertex <- pure $ rotateY ((event.clientX - oldX) / 1000.0) vertex
  clearRect ctx {x: -500.0 , y:  -500.0, width: 1000.0, height: 1000.0}
  project vertex ctx
  modify_(\_ -> vertex) ref

rotateX theta vertices = map (\v -> rotateYZ v) vertices
  where
    rotateYZ v = 
        let sina = sin theta
            cosa = cos theta
        in {x:v.x,y: v.y*cosa - v.z*sina, z: v.z *cosa + v.y*sina}

rotateY :: Number->Array Vertex -> Array Vertex
rotateY theta vertices = map (\v -> rotateXZ v) vertices
  where
    rotateXZ v = 
        let sina = sin theta
            cosa = cos theta
        in  {x:v.x * cosa - v.z * sina, y:v.y, z:v.z * cosa + v.x * sina }

project :: Array Vertex -> Context2D -> Effect Unit
project vertex ctx = do
  setStrokeStyle ctx "#6233ff"
  beginPath ctx
  drawLine (unsafePartial $ fromJust $ vertex !! 0) (unsafePartial $ fromJust $ vertex !! 1)
  drawLine (unsafePartial $ fromJust $ vertex !! 0) (unsafePartial $ fromJust $ vertex !! 2)
  drawLine (unsafePartial $ fromJust $ vertex !! 0) (unsafePartial $ fromJust $ vertex !! 4)
  drawLine (unsafePartial $ fromJust $ vertex !! 1) (unsafePartial $ fromJust $ vertex !! 5)
  drawLine (unsafePartial $ fromJust $ vertex !! 1) (unsafePartial $ fromJust $ vertex !! 3)
  drawLine (unsafePartial $ fromJust $ vertex !! 2) (unsafePartial $ fromJust $ vertex !! 3)
  drawLine (unsafePartial $ fromJust $ vertex !! 2) (unsafePartial $ fromJust $ vertex !! 6)
  drawLine (unsafePartial $ fromJust $ vertex !! 4) (unsafePartial $ fromJust $ vertex !! 6)
  drawLine (unsafePartial $ fromJust $ vertex !! 3) (unsafePartial $ fromJust $ vertex !! 7)
  drawLine (unsafePartial $ fromJust $ vertex !! 6) (unsafePartial $ fromJust $ vertex !! 7)
  drawLine (unsafePartial $ fromJust $ vertex !! 4) (unsafePartial $ fromJust $ vertex !! 5)
  drawLine (unsafePartial $ fromJust $ vertex !! 5) (unsafePartial $ fromJust $ vertex !! 7) 
  stroke ctx
  where
    drawLine v1 v2 = do 
      moveTo ctx v1.x v1.y
      lineTo ctx v2.x v2.y

