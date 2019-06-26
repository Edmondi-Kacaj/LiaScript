module Lia.Markdown.Quiz.MultipleChoice.View exposing (view, view_solution)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Lia.Markdown.Inline.Types exposing (Annotation, Inlines, MultInlines)
import Lia.Markdown.Inline.View exposing (annotation, view_inf)
import Lia.Markdown.Quiz.MultipleChoice.Types exposing (Quiz, State)
import Lia.Markdown.Quiz.MultipleChoice.Update exposing (Msg(..))


view : Quiz -> State -> Html Msg
view quiz state =
    quiz.options
        |> List.indexedMap Tuple.pair
        |> List.map2 (option False) state
        |> Html.table [ Attr.attribute "cellspacing" "8" ]


view_solution : Quiz -> Html Msg
view_solution quiz =
    quiz.options
        |> List.indexedMap Tuple.pair
        |> List.map2 (option True) quiz.solution
        |> Html.table [ Attr.attribute "cellspacing" "8" ]


option : Bool -> Bool -> ( Int, Inlines ) -> Html Msg
option solved checked ( id, line ) =
    Html.tr [ Attr.class "lia-check-item" ]
        [ Html.td [ Attr.attribute "valign" "top", Attr.class "lia-label" ]
            [ Html.input
                [ Attr.type_ "checkbox"
                , Attr.checked checked
                , if solved then
                    Attr.disabled True

                  else
                    onClick (Toggle id)
                ]
                []
            , Html.span [ Attr.class "lia-check-btn" ] [ Html.text "check" ]
            ]
        , line
            |> List.map view_inf
            |> Html.td [ Attr.class "lia-label" ]
        ]
