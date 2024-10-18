module Main exposing (..)

import Browser
import Html exposing (Html, audio, div, img, input, source, text)
import Html.Attributes exposing (autofocus, autoplay, class, height, src, style, value)
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
  | ShowingSuccessScreen Screen


type alias Trick =
  { boxtroll : String
  , background : String
  , text : String
  , successScreen : String
  }

type alias PickedTrick =
  { boxtroll : (Maybe String, List String)
  , background : (Maybe String, List String)
  , text : (Maybe String, List String)
  , successScreen : (Maybe String, List String)
  }

type alias Screen =
  String

boxtrolls : List String
boxtrolls = 
  [ "boxtroll_fish"
  , "boxtroll_fragile"
  , "boxtroll_oilcan"
  , "boxtroll_shoe"
  , "boxtroll_sparky"
  , "boxtroll_wheels"
  ]

backgrounds : List String
backgrounds =
  [ "bkg1"
  , "bkg2"
  , "bkg3"
  , "bkg4"
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
  ]

successScreens : List String
successScreens =
  [ "SuccessScreen01"
  , "SuccessScreen02"
  , "SuccessScreen03"
  , "SuccessScreen04"
  ]

easyTrick : Trick
easyTrick =
  Trick "boxtroll_fish" "bkg1" "Easy Trick 16 with lots of words. Oh my, so many words!" "SuccessScreen01"

init : () -> (Model, Cmd Msg)
init _ =
  ( WaitingForInput
  , Cmd.none
  )



-- UPDATE


type Msg 
  = KeyboardInput String
  | TrickPicked PickedTrick


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    KeyboardInput number -> 
      case number of
        "4" ->
          case model of
            WaitingForInput ->
              ( ShowingTrick easyTrick
              , Cmd.none
              )

            _ ->
              doNothingWith model
        
        "s" ->
          case model of
            ShowingTrick trick ->   
              ( ShowingSuccessScreen trick.successScreen
              , Cmd.none
              )

            _ -> 
              doNothingWith model

        "w" ->
          ( WaitingForInput
          , Cmd.none
          )

        _ ->
          case model of
            WaitingForInput ->
              ( model
              , Random.generate TrickPicked pickRandomTrick
              )

            _ ->
              doNothingWith model
    
    TrickPicked pickedTrick ->
      ( ShowingTrick (toTrick pickedTrick)
      , Cmd.none
      )


doNothingWith : Model -> (Model, Cmd Msg)
doNothingWith model = 
  ( model
  , Cmd.none
  )

pickRandomTrick : Random.Generator PickedTrick
pickRandomTrick =
  Random.map4 PickedTrick pickBoxtrolls pickBackgrounds pickTrickText pickSuccessScreens

pickBoxtrolls : Random.Generator (Maybe String, List String)
pickBoxtrolls =
  Random.List.choose boxtrolls

pickBackgrounds : Random.Generator (Maybe String, List String)
pickBackgrounds =
  Random.List.choose backgrounds

pickTrickText : Random.Generator (Maybe String, List String)
pickTrickText =
  Random.List.choose trickTexts

pickSuccessScreens : Random.Generator (Maybe String, List String)
pickSuccessScreens =
  Random.List.choose successScreens


toTrick : PickedTrick -> Trick
toTrick pickedTrick =
  { boxtroll = Maybe.withDefault "" (Tuple.first pickedTrick.boxtroll)
  , background = Maybe.withDefault "" (Tuple.first pickedTrick.background)
  , text = Maybe.withDefault "" (Tuple.first pickedTrick.text)
  , successScreen = Maybe.withDefault "" (Tuple.first pickedTrick.successScreen)
  }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div [ style "background-color" "black" ] 
      [ div [ style "display" "flex", style "width" "100%", style "justify-content" "center"] 
            [ input [autofocus True, value "", onInput KeyboardInput, style "background-color" "black" ] []]
      , case model of
          WaitingForInput ->
            div [] [ img ([ src (toImgRef "cover") ] ++ fullScreenImageStyle) []]
          
          ShowingTrick trick ->
            div [] 
                [ div [] 
                      [ img 
                          [ src (toImgRef trick.boxtroll)
                          , style "position" "absolute"
                          , style "bottom" "0px"
                          , style "left" "200px"
                          , style "width" "250px"
                          , style "z-index" "20"
                          ] 
                          []
                      , viewAudio trick.boxtroll
                      ] 
                , div [] 
                      [ img 
                          ([ src (toImgRef trick.background)
                          , style "z-index" "10" ] 
                          ++ fullScreenImageStyle
                          ) 
                          [] 
                      ] 
                , div [] 
                      [ Html.h2 
                          [ style "position" "absolute"
                          , style "top" "200px"
                          , style "right" "50px"
                          , style "width" "1350px"
                          , style "z-index" "30" 
                          , style "font-size" "100px"
                          , style "color" "LemonChiffon"
                          , style "text-shadow" "10px 10px 10px black"
                          , style "font-family" "Copperplate, Papyrus, fantasy"
                          , style "font-weight" "bold"
                          , style "word-wrap" "break-word"
                          , style "white-space" "normal"
                          ] 
                          [ text trick.text 
                          ] 
                      ]
                ]
          ShowingSuccessScreen screen ->
            div [] 
                [ img ([ src (toImgRef screen) ] ++ fullScreenImageStyle) [] 
                , viewAudio screen
                ]

      ]

viewAudio : String -> Html msg
viewAudio file = 
  audio [ autoplay True ]
        [ source [ src (toAudioRef file), Html.Attributes.type_ "audio/mpeg" ] [] ] 

fullScreenImageStyle : List (Html.Attribute msg)
fullScreenImageStyle =
  [style "position" "absolute", style "top" "25px", style "width" "100%"]

toImgRef : String -> String
toImgRef file =
  "img/" ++ file ++ ".jpg"

toAudioRef : String -> String
toAudioRef file =
  "aud/" ++ file ++ ".mp3"