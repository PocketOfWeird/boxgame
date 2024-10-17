module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (autofocus, value)
import Html.Events exposing (onInput)
import Random
import Random.List
import Tuple

main = 
  Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }



-- MODEL


type Model 
  = WaitingForInput
  | ShowingTrick Trick
  | ShowingSuccessClip Clip


type alias Trick =
  { boxtroll : (Maybe String, List String)
  , background : (Maybe String, List String)
  , text : (Maybe String, List String)
  , successClip : (Maybe String, List String)
  }

type alias Clip =
  String

boxtrolls : List String
boxtrolls = 
  [ "img/boxtroll_fish.jpg"
  , "img/boxtroll_fragile.jpg"
  , "img/boxtroll_oilcan.jpg"
  , "img/boxtroll_shoe.jpg"
  , "img/boxtroll_sparky.jpg"
  , "img/boxtroll_wheels.jpg"
  ]

backgrounds : List String
backgrounds =
  [ "img/bkg1.jpg"
  , "img/bkg2.jpg"
  , "img/bkg3.jpg"
  , "img/bkg4.jpg"
  ]

trickTexts : List String
trickTexts =
  [ "Blah Blah 1"
  , "Blah Blah 2"
  , "Blah Blah 3"
  , "Blah Blah 4"
  , "Blah Blah 5"
  , "Blah Blah 6"
  , "Blah Blah 7"
  , "Blah Blah 8"
  , "Blah Blah 9"
  , "Blah Blah 10"
  , "Blah Blah 11"
  , "Blah Blah 12"
  , "Blah Blah 13"
  , "Blah Blah 14"
  , "Blah Blah 15"
  , "Easy Trick 16"
  ]

successClips : List String
successClips =
  [ "01.mp4"
  , "02.mp4"
  , "03.mp4"
  , "04.mp4"
  , "05.mp4"
  , "06.mp4"
  ]

init : () -> (Model, Cmd Msg)
init _ =
  ( WaitingForInput
  , Cmd.none
  )



-- UPDATE


type Msg 
  = KeyboardInput String
  | TrickPicked Trick


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    KeyboardInput number -> 
      case number of
        "4" ->
          ( ShowingTrick (Trick (Just "img/boxtroll_wheels.jpg", []) (Just "img/bkg1.jpg", []) (Just "Easy Trick 16", []) (Just "06.mp4", []))
          , Cmd.none
          )
        
        "s" ->
          case model of
            ShowingTrick trick -> 
              case (Tuple.first trick.successClip) of
                Just successClip ->
                  ( ShowingSuccessClip successClip
                  , Cmd.none
                  )
                Nothing -> (model, Cmd.none)

            _ -> (model, Cmd.none)

        _ ->
          ( model
          , Random.generate TrickPicked pickRandomTrick
          )
    
    TrickPicked trick ->
      ( ShowingTrick trick
      , Cmd.none
      )




pickRandomTrick : Random.Generator Trick
pickRandomTrick =
  Random.map4 Trick sixBoxtrolls fourBackgrounds sixteenTricks sixSuccessClips

sixBoxtrolls : Random.Generator (Maybe String, List String)
sixBoxtrolls =
  Random.List.choose boxtrolls

fourBackgrounds : Random.Generator (Maybe String, List String)
fourBackgrounds =
  Random.List.choose backgrounds

sixteenTricks : Random.Generator (Maybe String, List String)
sixteenTricks =
  Random.List.choose trickTexts

sixSuccessClips : Random.Generator (Maybe String, List String)
sixSuccessClips =
  Random.List.choose successClips




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [] 
      [ div [] [ text "The BoxGame"]
      , div [] [ input [autofocus True, value "", onInput KeyboardInput] []]
      , case model of
          WaitingForInput ->
            div [] [ text "Waiting for input" ]
          
          ShowingTrick trick ->
            div [] 
                [ div [] [ text (Maybe.withDefault "boxtroll" (Tuple.first trick.boxtroll)) ] 
                , div [] [ text (Maybe.withDefault "background" (Tuple.first trick.background)) ] 
                , div [] [ text (Maybe.withDefault "text" (Tuple.first trick.text)) ] 
                , div [] [ text (Maybe.withDefault "successClip" (Tuple.first trick.successClip)) ] 
                ]
          ShowingSuccessClip clip ->
            div [] [ text ("Success Clip: " ++ clip) ]

      ]


