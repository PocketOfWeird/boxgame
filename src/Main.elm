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
  | PlayingDrums TrickWithDrum
  | ShowingSuccessScreen Screen


type alias Trick =
  { boxtroll : String
  , background : String
  , text : String
  , successScreen : String
  }

type alias TrickWithDrum =
  { trick : Trick 
  , drumSounds : List String
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
  [ "Do the boxtroll dance"
  , "Dance like a chicken"
  , "Do 3 jumping jacks"
  , "Say the ABC's backwards"
  , "Name 3 types of cheese"
  , "Bark like a little doggie"
  , "Spin around 2 times"
  , "Stand on one leg"
  , "Clap 7 times"
  , "Stick out your tongue"
  , "Hop 3 times"
  , "Do 1 squat"
  , "Count to 13"
  , "Count to 14"
  , "Count to 15"
  ]

successScreens : List String
successScreens =
  [ "SuccessScreen01"
  , "SuccessScreen02"
  , "SuccessScreen03"
  , "SuccessScreen04"
  , "SuccessScreen05"
  , "SuccessScreen06"
  , "SuccessScreen07"
  , "SuccessScreen08"
  , "SuccessScreen09"
  ]

easyTrick : Trick
easyTrick =
  Trick "boxtroll_fish" "bkg1" "Give us a high five!" "SuccessScreen01"

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
  case (msg, model) of

    (KeyboardInput number, WaitingForInput) -> 
      case number of
        "1" ->
          ( model
          , Random.generate TrickPicked pickRandomTrick 
          )

        "2" ->
          ( model
          , Random.generate TrickPicked pickRandomTrick 
          )

        "3" ->
          ( model
          , Random.generate TrickPicked pickRandomTrick 
          )

        "4" ->
          ( ShowingTrick easyTrick
          , Cmd.none
          )
        
        _ ->
          doNothingWith model
      
    (KeyboardInput keyboardInput, ShowingTrick trick) -> 
      TrickWithDrum trick [] |> PlayingDrums |> decidePlayingDrums keyboardInput

    (KeyboardInput keyboardInput, PlayingDrums trickWithDrum) ->
      decidePlayingDrums keyboardInput model

    (KeyboardInput number, ShowingSuccessScreen screen) -> 
      case number of
        "w" ->
          ( WaitingForInput
          , Cmd.none
          )
        _ ->
          doNothingWith model

    (TrickPicked pickedTrick, WaitingForInput) ->
      ( ShowingTrick (toTrick pickedTrick)
      , Cmd.none
      )
    
    _ ->
      doNothingWith model


decidePlayingDrums : String -> Model -> (Model, Cmd Msg)
decidePlayingDrums keyboardInput model =
  case model of 
    PlayingDrums trickWithDrum ->
      case keyboardInput of
        "1" ->
          ( PlayingDrums (updateTrickWithDrums trickWithDrum "drum01")
          , Cmd.none
          )
          
        "2" ->
          ( PlayingDrums (updateTrickWithDrums trickWithDrum "drum02")
          , Cmd.none
          )
        
        "3" ->
          ( PlayingDrums (updateTrickWithDrums trickWithDrum "drum03")
          , Cmd.none
          )
        
        "4" ->
          ( PlayingDrums (updateTrickWithDrums trickWithDrum "drum04")
          , Cmd.none
          )
        
        "s" -> 
          ( ShowingSuccessScreen trickWithDrum.trick.successScreen
          , Cmd.none
          )

        _ -> doNothingWith model

    _ -> doNothingWith model

updateTrickWithDrums : TrickWithDrum -> String -> TrickWithDrum
updateTrickWithDrums trickWithDrum newDrumSound =
  { trickWithDrum | drumSounds = trickWithDrum.drumSounds ++ [newDrumSound] 
  }


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
            viewShowingTrick trick True

          PlayingDrums trickWithDrum ->
            div []
                [ viewShowingTrick trickWithDrum.trick False
                , div [] (List.map viewAudio trickWithDrum.drumSounds)
                ]

          ShowingSuccessScreen screen ->
            div [] 
                [ img ([ src (toImgRef screen) ] ++ fullScreenImageStyle) [] 
                , viewAudio screen
                ]

      ]


viewShowingTrick : Trick -> Bool -> Html Msg
viewShowingTrick trick playSound =
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
            , if playSound then 
                viewAudio trick.boxtroll
              else
                div [] []
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
                , style "font-size" "150px"
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