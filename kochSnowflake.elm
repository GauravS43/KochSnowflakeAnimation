navy = rgb 0 0 200

myShapes model =
  [ case model.state of
      Dragging x -> circle 5 |> filled navy |> move (toPos x, -58)
      Waiting x -> circle 4 |> filled navy |> move (toPos x, -58)
      Animating x -> circle 4 |> outlined (solid 0.5) navy |> move (toPos x, -58)
  , rect 96 16 |> filled (rgba 237 147 144 0.75) |> move (-48, 56)
  , rect 96 98 |> filled (rgba 234 166 150 0.75) |> move (-48, -1)
  , drawExplanation -92 50 |> move (0, -5)
  , drawSnowflake model |> move (116, -10) |> scale 0.75
  , roundedRect 40 10 5 |> filled (rgba 246 148 193 0.75) |> move (48, -42)
    |> notifyTap (if model.text == "Start Animating" then StartAnimating else StopAnimating)
  , text model.text |> centered |> size 4 |> filled (rgb 100 100 200) |> move (48, -43.5)
    |> notifyTap (if model.text == "Start Animating" then StartAnimating else StopAnimating)
  , drawSampleCurve ((state2num model.state)/2 + 1) |> scale (0.9) |> move (-5, 0)
  , openPolygon [(0, 64), (0, -50)] |> outlined (solid 1) (rgb 243 116 174)
  , openPolygon [(-96, -50), (96, -50)] |> outlined (solid 1) (rgb 243 116 174)
  , openPolygon [(-96, -50), (-96, 64), (96, 64), (96,-50)] |> outlined (solid 2) (rgb 243 116 174)
  , openPolygon [(-96, 48), (0, 48)] |> outlined (solid 1) (rgb 243 116 174)
  , List.map ( \ idx -> String.fromInt idx |> text |> centered |> size 4 |> filled (rgb 100 100 200) |> move (2* (toPos (toFloat idx)) + 50,-59.5) )
      (List.range 0 5)
      |> group
  , roundedRect 110 10 5 |> filled (rgba 246 148 193 0.75)
      |> move (0,-58)
      |> ( case model.state of 
             Waiting _  -> notifyMouseDownAt StartDragAt
           {-  Dragging _ -> ( \ input -> input |> notifyMouseMoveAt MoveDragAt
                             |> notifyMouseUp StopDrag
                             |> notifyLeave StopDrag
                          ) -}
             Dragging _ -> notifyMouseMoveAt MoveDragAt
                             >> notifyMouseUp StopDrag
                             >> notifyLeave StopDrag
             Animating _ -> identity
         )
  ]

drawExplanation x y = group
  [ text "Koch Snowflake" |> centered |> size 10 |> filled black |> move (-48, 57)
  , text "For this fractal, we start with a triangle and create" |> alignLeft |> size 4 |> filled black |> move (x, y - 5)
  , text "three new triangles each iteration." |> alignLeft |> size 4 |> filled black |> move (x, y - 10)
  , text "Procedure:" |> alignLeft |> size 4 |> filled black |> move (x, y - 20)
  , text "First, we divide each side into three segments." |> alignLeft |> size 4 |> filled black |> move (x, y - 25)
  , text "Second, raise the middle to form an equilateral triangle." |> alignLeft |> size 4 |> filled black |> move (x, y - 30)
  , text "Repeat procedure for all sides." |> alignLeft |> size 4 |> filled black |> move (x, y - 35)
  , text "The diagram below shows how triangles are added." |> alignLeft |> size 4 |> filled black |> move (x, y - 45)
  , text "The blue line divides the three side segments" |> alignLeft |> size 4 |> filled black |> move (x, y - 50)
  , text "and moves to form a triangle." |> alignLeft |> size 4 |> filled black |> move (x, y - 55)
  ]    


drawSnowflake model = 
  let
    firstList = [(0,0), (90,0)]
  in
    group
    [ drawKoch ((state2num model.state)/2 + 1) 1 firstList 0.5 (rgb 100 100 200) |> move (-96, 0)
    , drawKoch ((state2num model.state)/2 + 1) 1 firstList 0.5 (rgb 100 100 200) |> rotate (degrees 120) |> move (-6, 0)
    , drawKoch ((state2num model.state)/2 + 1) 1 firstList 0.5 (rgb 100 100 200) |> rotate (degrees -120) |> move (-51, 78)
    ]

drawKoch limit n list width clr =
  group <|
     ( if n >= limit then
         [ openPolygon list |> outlined (solid width) clr ]
       else
         [ drawKoch limit (n+1) (addPoints limit n list []) width clr]
     )

addPoints limit n list newList = 
  case list of
    ([w]) ->
      newList ++ [w]
    (w::ws) ->
      case w of
        (x, y) ->
          addPoints limit n ws (newList ++ (auxAddPoints limit n (x, y) ws))
    (_) ->
      newList
  
auxAddPoints limit n (x0, y0) list = 
  case list of
    (w::ws) ->
      case w of 
        (x, y) ->
          let
            dx = (x - x0)
            dy = (y - y0)
            dd = sqrt (dx^2 + dy^2) 
            angle = (atan2 dy dx - pi/3)
            p1x = x0 + (dx/3)
            p1y = y0 + (dy/3)
            dl = if (limit - n) > 1 then 1 else (limit - n)
          in
          [ (x0,y0)
          , (p1x, p1y)
          , (p1x + (cos angle)*(dl*dd/3), (p1y + (sin angle)*(dl*dd/3)))
          , (x - (dx/3), y - (dy/3))
          ]
    (_) ->
      []
      
type alias Point = (Float,Float)

type Msg 
  = Tick Float GetKeyState
  | StartDragAt Point
  | MoveDragAt Point
  | StopDrag
  | StartAnimating
  | StopAnimating
  
type alias Model = { time : Float, text : String}

type State 
  = Waiting Float
  | Dragging Float
  | Animating Float
  
state2num state =
  case state of
    Waiting x -> x
    Dragging x -> x
    Animating x -> x
    
update msg model 
  = case msg of
      Tick t _   -> { model | time = t
                            , text = case model.state of
                                Animating old -> 
                                  let new = old + t - model.time
                                  in if new > 10 then "Start Animating" else "Stop Animating" 
                                otherwise -> "Start Animating"
                            , state = case model.state of 
                                Animating old -> 
                                  let new = old + t - model.time
                                  in if new > 10 then Waiting 10 else Animating new 
                                otherwise -> otherwise
                            }
      StartDragAt (x,_) -> 
        case model.state of 
          Waiting _ -> { model | state = Dragging (toGen x) } 
          _ -> model
      MoveDragAt (x,_) -> 
        case model.state of 
          Dragging _ -> { model | state = Dragging ( toGen x ) } 
          _ -> model
      StopDrag -> 
        case model.state of 
          Dragging generation -> { model | state = Waiting (toFloat <| round generation) } 
          _ -> model
      StartAnimating ->
        case model.state of 
          Waiting generation -> { model | text = "Stop Animating", state = Animating (if generation >= 10 then 0 else generation) } 
          _ -> model
      StopAnimating ->
        case model.state of 
          Animating generation -> { model | text = "Start Animating", state = Waiting generation } 
          _ -> model

drawSampleCurve state =
  let 
    delta = (state - toFloat (floor state))
    dy = (sqrt (32^2 - 16^2))/3
  in
    case (floor state) of
      (1) -> group
        [ openPolygon [(-96, -45), (0, -45)] |> outlined (solid 1) black
        , openPolygon [(-64, -45), (-32, -45)] |> outlined (solid 1) (rgba 21 244 238 delta)]
      (2) -> group
        [ drawKoch (state - 1) 1 [(-96, 45), (0, 45)] 1 (rgb 21 244 238) |> scaleY -1
        , openPolygon [(-96, -45), (-64, -45)] |> outlined (solid 1) black
        , openPolygon [(-32, -45), (0, -45)] |> outlined (solid 1) black]
      (3) -> group
        [ drawKoch (2) 1 [(-96, 45), (0, 45)] 1 (rgb 21 244 238) |> scaleY -1
        , openPolygon [(-96, -45), (-64, -45)] |> outlined (solid 1) black
        , openPolygon [(-64, -45), (-48, -45 + sqrt (32^2 - 16^2))] |> outlined (solid 1) (rgba 0 0 0 delta)
        , openPolygon [(-48, -45 + sqrt (32^2 - 16^2)), (-32, -45)] |> outlined (solid 1) (rgba 0 0 0 delta)
        , openPolygon [(-32, -45), (0, -45)] |> outlined (solid 1) black]
      (4) -> group
        [ drawKoch (2) 1 [(-96, 45), (0, 45)] 1 black |> scaleY -1
        , openPolygon [(-96 + 32/3, -45), (-64 - 32/3, -45)] |> outlined (solid 1) (rgba 21 244 238 delta)
        , openPolygon [(-32 + 32/3, -45), (0 - 32/3, -45)] |> outlined (solid 1) (rgba 21 244 238 delta)
        , openPolygon [(-64 + 32/3, -45 + 2*dy), (-48 - 32/3, -45 + dy)] |> outlined (solid 1) (rgba 21 244 238 delta)
        , openPolygon [(-48 + 32/3, -45 + dy), (-32 - 32/3, -45 + 2*dy)] |> outlined (solid 1) (rgba 21 244 238 delta)
        ]
      (_) -> group
        [ drawKoch (state - 3) 1 [(-96, 45), (0, 45)] 1 (rgb 21 244 238) |> scaleY -1
        , openPolygon [(-96, -45), (-96 + 32/3, -45)] |> outlined (solid 1) black
        , openPolygon [(-64 - 32/3, -45), (-64, -45)] |> outlined (solid 1) black
        , openPolygon [(-32 + 32/3, -45), (-32 , -45)] |> outlined (solid 1) black
        , openPolygon [(0 - 32/3, -45), (0 , -45)] |> outlined (solid 1) black
        , openPolygon [(-64, -45), (-48 - 32/3, -45 + dy)] |> outlined (solid 1) black
        , openPolygon [(-48, -45 + sqrt (32^2 - 16^2)), (-64 + 32/3, -45 + 2*dy)] |> outlined (solid 1) black
        , openPolygon [(-48, -45 + sqrt (32^2 - 16^2)), (-32 - 32/3, -45 + 2*dy)] |> outlined (solid 1) black
        , openPolygon [(-32, -45), (-48 + 32/3, -45 + dy)] |> outlined (solid 1) black]
        
-- convert mouse position to generation number and vice versa
toGen mouseX = 
  let
    raw = 0.1 * (mouseX + 50) 
  in
    if raw < 0 then 
      0
    else if raw > 10 then
      10
    else 
      raw
      
toPos genNum = 10 * genNum - 50

init = { state = Waiting 0
       , time = 0
       , text = "Play"
       }

main = gameApp Tick { model = init, view = view, update = update, title = "Game Slot" }

view model = collage 192 128 (myShapes model)



