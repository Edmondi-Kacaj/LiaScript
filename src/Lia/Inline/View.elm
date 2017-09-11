module Lia.Inline.View exposing (reference, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Lia.Effect.View as Effect
import Lia.Inline.Types exposing (Inline(..), Reference(..), Url(..))
import Lia.Utils


view : Int -> Inline -> Html msg
view visible element =
    case element of
        Code e ->
            Html.code [] [ Html.text e ]

        Chars e ->
            Html.text e

        Bold e ->
            Html.b [] [ view visible e ]

        Italic e ->
            Html.em [] [ view visible e ]

        Strike e ->
            Html.s [] [ view visible e ]

        Underline e ->
            Html.u [] [ view visible e ]

        Superscript e ->
            Html.sup [] [ view visible e ]

        Container list ->
            Html.span [] <| List.map (\e -> view visible e) list

        Ref e ->
            reference e

        Formula mode e ->
            Lia.Utils.formula mode e

        Symbol e ->
            Lia.Utils.stringToHtml e

        HTML e ->
            Lia.Utils.stringToHtml e

        EInline idx effect_name elements ->
            Effect.view (view visible) idx visible effect_name elements


reference : Reference -> Html msg
reference ref =
    let
        media url_ style_ =
            case style_ of
                Nothing ->
                    [ Attr.src <| get_url url_ ]

                Just s ->
                    [ Attr.src <| get_url url_
                    , Attr.attribute "style" s
                    ]
    in
    case ref of
        Link alt_ url_ ->
            Html.a [ Attr.href <| get_url url_ ] [ Html.text alt_ ]

        Image alt_ url_ style_ ->
            Html.img (media url_ style_) [ Html.text alt_ ]

        Movie alt_ url_ style_ ->
            Html.iframe (media url_ style_) [ Html.text alt_ ]


get_url : Url -> String
get_url url =
    case url of
        Full str ->
            str

        Mail str ->
            str

        Partial str ->
            str
