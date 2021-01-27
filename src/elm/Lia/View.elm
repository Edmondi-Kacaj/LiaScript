module Lia.View exposing (view)

import Flip exposing (flip)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Lia.Index.View as Index
import Lia.Markdown.Config as Config
import Lia.Markdown.Effect.Model exposing (current_paragraphs)
import Lia.Markdown.Effect.View exposing (responsive, state)
import Lia.Markdown.Inline.Stringify exposing (stringify)
import Lia.Markdown.Inline.View exposing (view_inf)
import Lia.Markdown.View as Markdown
import Lia.Model exposing (Model)
import Lia.Section exposing (SubSection)
import Lia.Settings.Model as Setting exposing (Mode(..))
import Lia.Settings.Update exposing (toggle_sound)
import Lia.Settings.View as Settings
import Lia.Update exposing (Msg(..), get_active_section)
import Port.Event exposing (Event)
import Port.Share exposing (share)
import Session exposing (Screen)
import Translations as Trans exposing (Lang)


{-| Main view for the entire LiaScript model with the parameters:

1.  `screen`: width and heigth of the window
2.  `hasShareAPI`: will enable sharing vie the `navigation.share` api, otherwise
    create an QR-code with the entire course-URL
3.  `hasIndex`: display a home-button or not
4.  `model`: the preprocessed LiaScript Model

-}
view : Screen -> Bool -> Bool -> Model -> Html Msg
view screen hasShareAPI hasIndex model =
    let
        sharing =
            if hasShareAPI then
                Just <| share model.title (stringify model.definition.comment) model.url

            else
                Nothing
    in
    Html.div
        (Settings.design model.settings)
        [ view_aside sharing model
        , view_article sharing screen hasIndex model
        ]


{-| **@private:** Display the aside section that contains the document search,
table of contents and settings.
-}
view_aside : Maybe Event -> Model -> Html Msg
view_aside sharing model =
    Html.aside
        [ Attr.class "lia-toc"
        , Attr.style "max-width" <|
            if model.settings.table_of_contents then
                "280px"

            else
                "0px"
        ]
        [ Html.map UpdateIndex <| Index.view_search model.translation model.index_model
        , model.sections
            |> Index.view model.translation model.section_active
            |> Html.map Script
        , model
            |> get_active_section
            |> Maybe.andThen .definition
            |> Maybe.withDefault model.definition
            |> Settings.view model.settings
                model.url
                model.origin
                model.translation
                sharing
            |> Html.map UpdateSettings
        ]


{-| **@private:** show the current section, with navigation on top as well as a
footer, if it is required by the current display mode.
-}
view_article : Maybe Event -> Screen -> Bool -> Model -> Html Msg
view_article sharing screen hasIndex model =
    case get_active_section model of
        Just section ->
            Html.article [ Attr.class "lia-slide" ]
                [ section
                    |> .effect_model
                    |> state
                    |> view_nav
                        sharing
                        model.translation
                        hasIndex
                        model.settings
                        model.section_active
                , Config.init
                    model.settings.mode
                    section
                    model.section_active
                    model.settings.editor
                    model.translation
                    model.settings.light
                    (if model.settings.table_of_contents then
                        { screen | width = screen.width - 260 }

                     else
                        screen
                    )
                    |> Markdown.view
                    |> Html.map UpdateMarkdown
                , view_footer
                    model.translation
                    model.settings.sound
                    model.settings.mode
                    model.section_active
                    section.effect_model
                ]

        Nothing ->
            Html.text "no content"


{-| **@private:** used to diplay the text2speech output settings and spoken
comments in text, depending on the currently applied rendering mode.
-}
view_footer : Lang -> Bool -> Mode -> Int -> Lia.Markdown.Effect.Model.Model SubSection -> Html Msg
view_footer lang sound mode slide effects =
    case mode of
        Slides ->
            effects
                |> current_paragraphs
                |> List.map
                    (Tuple.second
                        >> List.map (view_inf effects.javascript lang)
                        >> Html.p []
                        >> Html.map (Tuple.pair slide >> Script)
                    )
                |> flip List.append [ responsive lang sound (UpdateSettings toggle_sound) ]
                |> Html.footer [ Attr.class "lia-footer" ]

        Presentation ->
            Html.footer [ Attr.class "lia-footer" ] [ responsive lang sound (UpdateSettings toggle_sound) ]

        Textbook ->
            Html.text ""


{-| **@private:** create a navigation button with:

1.  `str`: string to be displayed in the body
2.  `title`: attribute
3.  `id`: so that it can be identfied by external css
4.  `msg`: to release if pressed

-}
navButton : String -> String -> String -> msg -> Html msg
navButton str title id msg =
    Html.button
        [ onClick msg
        , Attr.title title
        , Attr.class "lia-btn lia-control lia-slide-control lia-left"
        , Attr.id id
        ]
        [ Html.text str ]


{-| **@private:** the navigation abr:

1.  `sharing`: if the share-API is available, then release this event
2.  `lang`: used for translations
3.  `hasIndex`: display home/index button
4.  `settings`: global LiaScript Settings
5.  `section_active`: section id to display
6.  `state`: fragments, if animations are active, not visible in textbook mode

-}
view_nav : Maybe Event -> Lang -> Bool -> Setting.Model -> Int -> String -> Html Msg
view_nav sharing lang hasIndex settings section_active state =
    Html.nav [ Attr.class "lia-toolbar", Attr.id "lia-toolbar-nav" ]
        [ Html.map UpdateSettings <| Settings.toggle_button_toc lang
        , if hasIndex then
            navButton "home" "index" "lia-btn-home" Home

          else
            Html.text ""
        , Html.span [ Attr.class "lia-spacer", Attr.id "lia-spacer-left" ] []
        , navButton "navigate_before" (Trans.basePrev lang) "lia-btn-prev" PrevSection
        , Html.span [ Attr.class "lia-labeled lia-left", Attr.id "lia-label-section" ]
            [ Html.span
                [ Attr.class "lia-label"
                , if settings.speaking then
                    Attr.style "text-decoration" "underline"

                  else
                    Attr.style "" ""
                ]
                [ Html.text (String.fromInt (section_active + 1))
                , Html.text <|
                    case settings.mode of
                        Textbook ->
                            ""

                        _ ->
                            state
                ]
            ]
        , navButton "navigate_next"
            (Trans.baseNext lang)
            "lia-btn-next"
            NextSection
        , Html.span [ Attr.class "lia-spacer", Attr.id "lia-spacer-left" ] []
        , Html.map UpdateSettings (Settings.switch_button_mode lang settings.mode)
        , Html.span [ Attr.class "lia-spacer", Attr.id "lia-spacer-left" ] []
        , Html.map UpdateSettings (Settings.buttonSettings lang settings)
        , Html.map UpdateSettings (Settings.buttonInfo lang settings)
        , Html.map UpdateSettings (Settings.buttonTranslation lang settings)
        , Html.map UpdateSettings (Settings.buttonShare lang sharing settings)
        ]
