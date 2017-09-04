module Lia exposing (..)

import Array
import Html exposing (Html)
import Lia.Code.Model as Code
import Lia.Effect.Model as Effect
import Lia.Index.Model as Index
import Lia.Model
import Lia.Parser
import Lia.Types
import Lia.Update
import Lia.View


type alias Model =
    Lia.Model.Model


type alias Msg =
    Lia.Update.Msg


type alias Mode =
    Lia.Types.Mode


init : Mode -> String -> Model
init mode script =
    parse <|
        { script = ""
        , error = ""
        , slides = []
        , quiz_model = Array.empty
        , code_model = Array.empty
        , current_slide = 0
        , mode = mode
        , effect_model = Effect.init "US English Male" Nothing
        , narator = "US English Male"
        , contents = True
        , index = Index.init []
        }


set_script : Model -> String -> Model
set_script model script =
    { model | script = script }


init_plain : String -> Model
init_plain =
    init Lia.Types.Plain


init_slides : String -> Model
init_slides =
    init Lia.Types.Slides


parse : Model -> Model
parse model =
    case Lia.Parser.run model.script of
        Ok ( slides, codes, quiz_vector, narator ) ->
            { model
                | slides = slides
                , error = ""
                , quiz_model = quiz_vector
                , index = Index.init slides
                , effect_model = Effect.init narator <| List.head slides
                , code_model = Code.init codes
                , narator =
                    if narator == "" then
                        "US English Male"
                    else
                        narator
            }

        Err msg ->
            { model | error = msg }


view : Model -> Html Msg
view model =
    Lia.View.view model


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    Lia.Update.update


switch_mode : Mode -> Model -> Model
switch_mode mode model =
    { model | mode = mode }


plain_mode : Model -> Model
plain_mode =
    switch_mode Lia.Types.Plain


slide_mode : Model -> Model
slide_mode =
    switch_mode Lia.Types.Slides
