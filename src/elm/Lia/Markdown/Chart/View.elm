module Lia.Markdown.Chart.View exposing
    ( getColor
    , view
    , viewBarChart
    , viewChart
    , viewHeatMap
    , viewMapChart
    , viewPieChart
    , viewRadarChart
    )

import Char exposing (isLower, toLower)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Encode as JE
import Lia.Markdown.Chart.Types exposing (Chart, Diagram(..))
import Lia.Markdown.HTML.Attributes exposing (Parameters, annotation)


view : Parameters -> Bool -> Chart -> Html msg
view attr light =
    encode True >> eCharts attr light Nothing


viewChart : Parameters -> Bool -> Chart -> Html msg
viewChart attr light =
    encode False >> eCharts attr light Nothing


viewBarChart : Parameters -> Bool -> Maybe String -> List String -> List ( Maybe String, List (Maybe Float) ) -> Html msg
viewBarChart attr light title category data =
    encodeBarChart title category data
        |> eCharts attr light Nothing


viewRadarChart : Parameters -> Bool -> Maybe String -> List String -> List ( String, List (Maybe Float) ) -> Html msg
viewRadarChart attr light title category data =
    encodeRadarChart title category data
        |> eCharts attr light Nothing


viewMapChart : Parameters -> Bool -> Maybe String -> List ( String, Maybe Float ) -> Maybe String -> Html msg
viewMapChart attr light title data json =
    encodeMapChart title data json
        |> eCharts attr light json


viewPieChart :
    Int
    -> Parameters
    -> Bool
    -> Maybe String
    -> Maybe (List String)
    -> List (List ( String, Float ))
    -> Html msg
viewPieChart width attr light title subtitle data =
    encodePieChart width title subtitle data
        |> eCharts attr light Nothing


viewHeatMap :
    Parameters
    -> Bool
    -> Maybe String
    -> List String
    -> List String
    -> List (List ( Int, Int, Maybe Float ))
    -> Html msg
viewHeatMap attr light title x y data =
    encodeHeatMap title x y data
        |> eCharts attr light Nothing


eCharts : Parameters -> Bool -> Maybe String -> JE.Value -> Html msg
eCharts attr light json option =
    Html.node "e-charts"
        (List.append
            [ Attr.attribute "mode" <|
                if light then
                    ""

                else
                    "dark"
            , option
                |> JE.encode 0
                |> Attr.attribute "option"
            , json
                |> Maybe.withDefault ""
                |> Attr.attribute "json"
            ]
            (annotation "lia-chart" attr)
        )
        []


encodeBarChart : Maybe String -> List String -> List ( Maybe String, List (Maybe Float) ) -> JE.Value
encodeBarChart xLabel category data =
    let
        bars =
            data
                |> List.foldl
                    (\( label_, floats ) bs ->
                        if List.all ((==) Nothing) floats then
                            bs

                        else
                            JE.object
                                [ ( "type", JE.string "bar" )
                                , ( "name"
                                  , label_
                                        |> Maybe.map JE.string
                                        |> Maybe.withDefault JE.null
                                  )
                                , ( "barGap", JE.int 0 )
                                , label
                                , ( "data"
                                  , floats
                                        |> List.map (Maybe.map JE.float >> Maybe.withDefault JE.null)
                                        |> JE.list identity
                                  )
                                ]
                                :: bs
                    )
                    []
                |> JE.list identity
    in
    JE.object
        [ ( "xAxis"
          , JE.object
                ([ ( "type", JE.string "category" )

                 --, ( "name", JE.string xLabel )
                 , ( "data"
                   , category
                        |> JE.list JE.string
                   )
                 ]
                    ++ (xLabel
                            |> Maybe.map (\title -> [ ( "name", JE.string title ) ])
                            |> Maybe.withDefault []
                       )
                )
          )
        , ( "yAxis"
          , JE.object [ ( "type", JE.string "value" ) ]
          )

        --, ( "title", JE.object [ ( "text", JE.string chart.title ) ] )
        , ( "legend"
          , JE.object
                [ ( "data"
                  , data
                        |> List.unzip
                        |> Tuple.first
                        |> List.filterMap identity
                        |> JE.list JE.string
                  )
                ]
          )
        , toolbox Nothing { saveAsImage = True, dataView = True, dataZoom = True, magicType = True }

        --  , brush
        , ( "tooltip", JE.object [] )
        , ( "series", bars )
        ]


encodeMapChart : Maybe String -> List ( String, Maybe Float ) -> Maybe String -> JE.Value
encodeMapChart title data json =
    let
        ( min, max ) =
            data
                |> List.filterMap Tuple.second
                |> List.foldl
                    (\value ( min_, max_ ) ->
                        ( if value < min_ then
                            value

                          else
                            min_
                        , if value > max_ then
                            value

                          else
                            max_
                        )
                    )
                    ( 9999999999, 0 )
    in
    [ ( "series"
      , [ JE.object
            [ ( "type", JE.string "map" )
            , ( "map"
              , json
                    |> Maybe.withDefault ""
                    |> JE.string
              )
            , ( "data"
              , data
                    |> List.filterMap
                        (\( key, value ) ->
                            case value of
                                Just num ->
                                    [ ( "name", JE.string key )
                                    , ( "value", JE.float num )
                                    ]
                                        |> JE.object
                                        |> Just

                                Nothing ->
                                    Nothing
                        )
                    |> JE.list identity
              )
            ]
        ]
            |> JE.list identity
      )
    , ( "visualMap"
      , JE.object
            [ ( "min", JE.float min )
            , ( "max", JE.float max )
            , ( "calculable", JE.bool True )
            , ( "itemHeight", JE.string "150px" )
            , ( "right", JE.string "0" )
            , ( "bottom", JE.string "center" )
            , ( "orient", JE.string "vertical" )
            ]
      )
    , toolbox Nothing { saveAsImage = True, dataView = True, dataZoom = False, magicType = False }
    , ( "tooltip", JE.object [] )
    ]
        ++ (title |> Maybe.map encodeTitle |> Maybe.withDefault [])
        |> JE.object


calcMax : List { x | max : Float } -> List (Maybe Float) -> List { x | max : Float }
calcMax =
    List.map2
        (\i d ->
            let
                value =
                    Maybe.withDefault 0 d
            in
            if value > i.max then
                { i | max = value }

            else
                i
        )


encodeRadarChart : Maybe String -> List String -> List ( String, List (Maybe Float) ) -> JE.Value
encodeRadarChart title category data =
    let
        max_ =
            category
                |> List.map (\c -> { name = c, max = 0 })

        indicator =
            data
                |> List.map Tuple.second
                |> List.foldl (\d i -> calcMax i d) max_
                |> List.map
                    (\i ->
                        JE.object
                            [ ( "name", JE.string i.name )
                            , ( "max", JE.float i.max )
                            ]
                    )
                |> JE.list identity

        values =
            data
                |> List.map
                    (\( name_, value ) ->
                        JE.object
                            [ ( "name", JE.string name_ )
                            , ( "value"
                              , value
                                    |> List.map (Maybe.withDefault 0 >> JE.float)
                                    |> JE.list identity
                              )
                            ]
                    )
                |> JE.list identity
    in
    [ ( "radar"
      , JE.object
            [ ( "indicator", indicator )
            , ( "name"
              , JE.object
                    [ ( "textStyle"
                      , JE.object
                            [ ( "color", JE.string "#fff" )
                            , ( "backgroundColor", JE.string "#999" )
                            , ( "borderRadius", JE.int 3 )
                            , ( "padding", JE.list JE.int [ 3, 5 ] )
                            ]
                      )
                    ]
              )
            ]
      )
    , toolbox Nothing { saveAsImage = True, dataView = True, dataZoom = False, magicType = False }
    , ( "tooltip", JE.object [] )
    , ( "series"
      , [ JE.object
            [ ( "type", JE.string "radar" )
            , ( "data", values )
            , ( "emphasis"
              , JE.object
                    [ ( "lineStyle"
                      , JE.object
                            [ ( "width", JE.int 4 )
                            ]
                      )
                    ]
              )
            ]
        ]
            |> JE.list identity
      )
    ]
        ++ (title |> Maybe.map encodeTitle |> Maybe.withDefault [])
        |> JE.object


encodeTitle : String -> List ( String, JE.Value )
encodeTitle title =
    [ ( "title"
      , JE.object
            [ ( "text", JE.string title ) ]
      )
    ]


encodeLegend : List ( String, JE.Value ) -> List String -> ( String, JE.Value )
encodeLegend params data =
    ( "legend"
    , ( "data", JE.list JE.string data )
        :: params
        |> JE.object
    )


encodeHeatMap : Maybe String -> List String -> List String -> List (List ( Int, Int, Maybe Float )) -> JE.Value
encodeHeatMap title xAxis yAxis data =
    let
        ( min, max ) =
            data
                |> List.concat
                |> List.foldl
                    (\( _, _, value ) ( min_, max_ ) ->
                        let
                            v =
                                Maybe.withDefault 0 value
                        in
                        ( if v < min_ then
                            v

                          else
                            min_
                        , if v > max_ then
                            v

                          else
                            max_
                        )
                    )
                    ( 9999999999, 0 )
    in
    [ toolbox (Just "7%") { saveAsImage = True, dataView = True, dataZoom = False, magicType = False }
    , ( "tooltip", JE.object [] )
    , ( "grid"
      , JE.object <|
            if title == Nothing then
                [ ( "height", JE.string "82%" )
                , ( "top", JE.string "0%" )

                --, ( "width", JE.string "100%" )
                ]

            else
                [ ( "height", JE.string "74%" )
                , ( "top", JE.string "7%" )

                --, ( "width", JE.string "100%" )
                ]
      )
    , ( "xAxis"
      , JE.object
            [ ( "type", JE.string "category" )
            , ( "data", JE.list JE.string xAxis )
            , ( "splitArea", JE.object [ ( "show", JE.bool True ) ] )
            ]
      )
    , ( "yAxis"
      , JE.object
            [ ( "type", JE.string "category" )
            , ( "data", JE.list JE.string yAxis )
            , ( "splitArea", JE.object [ ( "show", JE.bool True ) ] )
            ]
      )
    , ( "visualMap"
      , JE.object
            [ ( "min", JE.float min )
            , ( "max", JE.float max )
            , ( "calculable", JE.bool True )

            --, ( "width", JE.string "300px" )
            , ( "itemHeight", JE.string "150px" )
            , ( "right", JE.string "7%" )
            , ( "bottom", JE.string "9" )
            , ( "orient", JE.string "horizontal" )
            ]
      )
    , ( "series"
      , [ JE.object
            [ ( "type", JE.string "heatmap" )
            , ( "label", JE.object [ ( "show", JE.bool True ) ] )
            , ( "data"
              , data
                    |> List.concat
                    |> List.map
                        (\( x, y, z ) ->
                            JE.list identity
                                [ JE.int x
                                , JE.int y
                                , z |> Maybe.map JE.float |> Maybe.withDefault JE.null
                                ]
                        )
                    |> JE.list identity
              )
            , ( "emphasis"
              , JE.object
                    [ ( "itemStyle"
                      , JE.object
                            [ ( "shadowBlur", JE.int 10 )
                            , ( "shadowColor", JE.string "rgba(0, 0, 0, 0.5)" )
                            ]
                      )
                    ]
              )
            ]
        ]
            |> JE.list identity
      )
    ]
        ++ (if title == Nothing then
                []

            else
                [ ( "title"
                  , JE.object
                        [ ( "text"
                          , title |> Maybe.withDefault "" |> JE.string
                          )
                        , ( "left", JE.string "center" )
                        ]
                  )
                ]
           )
        |> JE.object


encodePieChart : Int -> Maybe String -> Maybe (List String) -> List (List ( String, Float )) -> JE.Value
encodePieChart width title subtitle data =
    if List.length data == 1 then
        let
            pieces =
                data
                    |> List.head
                    |> Maybe.withDefault []
                    |> List.map
                        (\( name_, value_ ) ->
                            JE.object
                                [ ( "name", JE.string name_ )
                                , ( "value", JE.float value_ )
                                ]
                        )
                    |> JE.list identity

            head =
                if title /= Nothing || subtitle /= Nothing then
                    [ ( "title"
                      , JE.object
                            [ ( "text"
                              , title |> Maybe.withDefault "" |> JE.string
                              )
                            , ( "subtext", subtitle |> Maybe.withDefault [] |> List.head |> Maybe.withDefault "" |> JE.string )
                            , ( "left", JE.string "center" )
                            ]
                      )
                    ]

                else
                    []
        in
        [ ( "series"
          , [ JE.object
                [ ( "type", JE.string "pie" )
                , ( "name", subtitle |> Maybe.andThen List.head |> Maybe.withDefault "" |> JE.string )

                --, ( "roseType", JE.string "radius" )
                , ( "radius"
                  , JE.string <|
                        if title /= Nothing || subtitle /= Nothing then
                            "65%"

                        else
                            "75%"
                  )
                , ( "center", JE.string "50%" )
                , ( "selectedMode", JE.string "single" )
                , ( "data", pieces )
                ]
            ]
                |> JE.list identity
          )
        , toolbox Nothing
            { saveAsImage = True
            , dataView = True
            , dataZoom = False
            , magicType = False
            }

        --, ( "legend"
        --  , JE.object
        --        [ ( "data"
        --          , data
        --                |> List.map Tuple.first
        --                |> JE.list JE.string
        --          )
        --, ( "right", JE.int 0 )
        --        , ( "top", JE.int 28 )
        --        ]
        --  )
        --  , brush
        , ( "tooltip"
          , JE.object
                [ ( "trigger", JE.string "item" )
                , ( "formatter"
                  , JE.string "{b} : {c} ({d}%)"
                    -- "{a}<br/>{b} : {c} ({d}%)"
                  )
                ]
          )
        ]
            |> List.append head
            |> JE.object

    else
        encodePieCharts width title subtitle data


encodePieCharts : Int -> Maybe String -> Maybe (List String) -> List (List ( String, Float )) -> JE.Value
encodePieCharts width title subtitle data =
    let
        relWidth =
            (toFloat width
                / (5.9 * toFloat (List.length data))
                |> (\w ->
                        if w > 72 then
                            72

                        else
                            w
                   )
                |> String.fromFloat
            )
                ++ "%"

        step =
            100 / toFloat (2 * List.length data)

        categories =
            data
                |> List.head
                |> Maybe.map (List.map Tuple.first)
                |> Maybe.withDefault []

        pieces =
            data
                |> List.indexedMap
                    (\i x ->
                        JE.object
                            [ ( "type", JE.string "pie" )

                            --, ( "name", subtitle |> Maybe.withDefault "" |> JE.string )
                            --, ( "roseType", JE.string "radius" )
                            , ( "radius"
                              , JE.string <|
                                    if title /= Nothing || subtitle /= Nothing then
                                        relWidth

                                    else
                                        "75%"
                              )
                            , ( "center"
                              , [ String.fromFloat (toFloat (2 * i) * step + step)
                                    ++ "%"
                                , "50%"
                                ]
                                    |> JE.list JE.string
                              )
                            , ( "label"
                              , JE.object
                                    [ ( "normal"
                                      , JE.object
                                            [ ( "formatter", JE.string "{c}" )
                                            , ( "position", JE.string "inside" )
                                            ]
                                      )
                                    ]
                              )
                            , ( "selectedMode", JE.string "single" )
                            , ( "data"
                              , x
                                    |> List.map
                                        (\( name_, value_ ) ->
                                            JE.object
                                                [ ( "name", JE.string name_ )
                                                , ( "value", JE.float value_ )
                                                ]
                                        )
                                    |> JE.list identity
                              )
                            ]
                    )

        head =
            if title /= Nothing || subtitle /= Nothing then
                [ ( "title"
                  , subtitle
                        |> Maybe.withDefault []
                        |> List.indexedMap
                            (\i sub ->
                                JE.object
                                    [ ( "subtext", JE.string sub )
                                    , ( "bottom", JE.int 40 )
                                    , ( "textAlign", JE.string "center" )
                                    , ( "left"
                                      , String.fromFloat (toFloat (2 * i) * step + step)
                                            ++ "%"
                                            |> JE.string
                                      )
                                    ]
                            )
                        |> (::)
                            (JE.object
                                [ ( "text"
                                  , title |> Maybe.withDefault "" |> JE.string
                                  )
                                , ( "left", JE.string "center" )
                                ]
                            )
                        |> JE.list identity
                  )
                ]

            else
                []
    in
    [ ( "series", JE.list identity pieces )
    , toolbox Nothing { saveAsImage = True, dataView = True, dataZoom = False, magicType = False }

    --, ( "legend"
    --  , JE.object
    --        [ ( "data"
    --          , data
    --                |> List.map Tuple.first
    --                |> JE.list JE.string
    --          )
    --, ( "right", JE.int 0 )
    --        , ( "top", JE.int 28 )
    --        ]
    --  )
    --  , brush
    , encodeLegend [ ( "top", JE.string "30px" ) ] categories
    , ( "tooltip"
      , JE.object
            [ ( "trigger", JE.string "item" )
            , ( "formatter"
              , JE.string "{b} : {c} ({d}%)"
                -- "{a}<br/>{b} : {c} ({d}%)"
              )
            ]
      )
    ]
        |> List.append head
        |> JE.object


label : ( String, JE.Value )
label =
    ( "label"
    , JE.object
        [ ( "normal"
          , JE.object
                [ ( "show", JE.bool False )
                , ( "position", JE.string "top" )
                ]
          )
        ]
    )


encode : Bool -> Chart -> JE.Value
encode withColor chart =
    JE.object
        [ ( "textStyle"
          , JE.object
                [ ( "fontFamily", JE.string "Roboto" )
                ]
          )
        , ( "xAxis"
          , JE.object
                [ ( "type", JE.string "value" )
                , ( "name", JE.string chart.xLabel )
                ]
          )
        , ( "yAxis"
          , JE.object
                [ ( "type", JE.string "value" )
                , ( "name", JE.string chart.yLabel )
                ]
          )
        , ( "title", JE.object [ ( "text", JE.string chart.title ) ] )
        , ( "legend"
          , JE.object
                [ ( "data", JE.list JE.string chart.legend )

                --, ( "right", JE.int 0 )
                , ( "top", JE.int 28 )
                ]
          )
        , toolbox Nothing { saveAsImage = True, dataView = True, dataZoom = True, magicType = True }

        --  , brush
        , ( "tooltip", JE.object [] )
        , ( "series"
          , chart.diagrams
                |> Dict.toList
                |> JE.list (series withColor)
          )
        ]


brush : ( String, JE.Value )
brush =
    ( "brush"
    , JE.object
        [ ( "toolbox"
          , JE.list JE.string [ "rect", "polygon", "lineX", "lineY", "keep", "clear" ]
          )
        ]
    )


toolbox : Maybe String -> { saveAsImage : Bool, dataView : Bool, dataZoom : Bool, magicType : Bool } -> ( String, JE.Value )
toolbox position config =
    ( "toolbox"
    , JE.object
        [ ( "bottom", JE.int 8 )
        , ( "left"
          , position
                |> Maybe.withDefault "center"
                |> JE.string
          )
        , ( "feature"
          , []
                |> List.append
                    (if config.saveAsImage then
                        [ ( "saveAsImage", JE.object [ ( "title", JE.string "store" ) ] ) ]

                     else
                        []
                    )
                |> List.append
                    (if config.dataView then
                        [ ( "dataView"
                          , JE.object
                                [ ( "title", JE.string "edit" )
                                , ( "lang", JE.list JE.string [ "data view", "turn off", "refresh" ] )
                                ]
                          )
                        ]

                     else
                        []
                    )
                |> List.append
                    (if config.dataZoom then
                        [ ( "dataZoom"
                          , JE.object
                                [ ( "title"
                                  , JE.object
                                        [ ( "zoom", JE.string "zoom" )
                                        , ( "back", JE.string "back" )
                                        ]
                                  )
                                ]
                          )
                        ]

                     else
                        []
                    )
                |> List.append
                    (if config.magicType then
                        [ ( "magicType"
                          , JE.object
                                [ ( "type"
                                  , JE.list JE.string
                                        [ "tiled"
                                        , "line"
                                        , "bar"
                                        ]
                                  )
                                , ( "title"
                                  , JE.object
                                        [ ( "stack", JE.string "stack" )
                                        , ( "tiled", JE.string "tiled" )
                                        , ( "line", JE.string "line" )
                                        , ( "bar", JE.string "bar" )
                                        ]
                                  )
                                ]
                          )
                        ]

                     else
                        []
                    )
                |> JE.object
          )
        ]
    )


series : Bool -> ( Char, Diagram ) -> JE.Value
series withColor ( char, diagram ) =
    JE.object <|
        List.append
            ([ symbol char
             , symbolSize char
             , label
             ]
                ++ (if withColor then
                        [ color char ]

                    else
                        []
                   )
            )
        <|
            case diagram of
                Lines list label_ ->
                    [ ( "data"
                      , list
                            |> List.map (\point -> JE.list JE.float [ point.x, point.y ])
                            |> JE.list identity
                      )
                    , ( "type", JE.string "line" )
                    , ( "barGap", JE.int 0 )
                    , style withColor char
                    , smooth withColor char
                    ]
                        ++ name label_

                Dots list label_ ->
                    [ ( "data"
                      , list
                            |> List.map (\point -> JE.list JE.float [ point.x, point.y ])
                            |> JE.list identity
                      )
                    , ( "barGap", JE.int 0 )
                    , ( "type", JE.string "scatter" )
                    ]
                        ++ name label_


name : Maybe String -> List ( String, JE.Value )
name label_ =
    case label_ of
        Nothing ->
            []

        Just str ->
            [ ( "name", JE.string str ) ]


style : Bool -> Char -> ( String, JE.Value )
style withColor char =
    ( "lineStyle"
    , JE.object
        [ ( "type"
          , JE.string <|
                if withColor then
                    if modBy 7 (Char.toCode char) == 0 then
                        "dashed"

                    else if modBy 5 (Char.toCode char) == 0 then
                        "dotted"

                    else
                        "solid"

                else
                    "solid"
          )
        ]
    )


smooth : Bool -> Char -> ( String, JE.Value )
smooth withColor char =
    ( "smooth"
    , if withColor then
        char
            |> Char.toCode
            |> modBy 2
            |> (==) 0
            |> JE.bool

      else
        JE.bool False
    )


symbolSize : Char -> ( String, JE.Value )
symbolSize c =
    ( "symbolSize"
    , JE.int <|
        if Char.isLower c then
            5

        else
            10
    )


symbol : Char -> ( String, JE.Value )
symbol c =
    ( "symbol"
    , JE.string <|
        case c of
            'd' ->
                "diamond"

            'D' ->
                "diamond"

            't' ->
                "triangle"

            'T' ->
                "triangle"

            'A' ->
                "arrow"

            'v' ->
                "triangle"

            'V' ->
                "triangle"

            '#' ->
                "rect"

            'H' ->
                "rect"

            'B' ->
                "roundRect"

            'N' ->
                "roundRect"

            'p' ->
                "pin"

            'P' ->
                "pin"

            '+' ->
                "diamond"

            'x' ->
                "rect"

            'X' ->
                "rect"

            _ ->
                "circle"
    )


colors : Dict Char String
colors =
    [ ( '*', "#000000" )
    , ( '+', "#000000" )
    , ( 'x', "#000000" )
    , ( 'a', "#FFBF00" ) -- Amber
    , ( 'b', "#0000FF" ) -- Blue
    , ( 'c', "#00FFFF" ) -- Cyan
    , ( 'd', "#8B0000" ) -- Dark red
    , ( 'e', "#555D50" ) -- Ebony
    , ( 'f', "#014421" ) -- Forest green
    , ( 'g', "#008000" ) -- Green
    , ( 'h', "#DF73FF" ) -- Heliotrope
    , ( 'i', "#4B0082" ) -- Indigo
    , ( 'j', "#00A86B" ) -- Jade
    , ( 'k', "#C3B091" ) -- Kaki
    , ( 'l', "#00FF00" ) -- Lime
    , ( 'm', "#3EB489" ) -- Mint
    , ( 'n', "#88540B" ) -- browN
    , ( 'o', "#FF7F00" ) -- Orange
    , ( 'p', "#FFC0CB" ) -- Pink
    , ( 'q', "#436B95" ) -- Queen blue
    , ( 'r', "#FF0000" ) -- Red
    , ( 's', "#C0C0C0" ) -- Silver
    , ( 't', "#008080" ) -- Teal
    , ( 'u', "#3F00FF" ) -- Ultramarine
    , ( 'v', "#EE82EE" )
    , ( 'w', "#FFFFFF" )
    , ( 'y', "#FFFF00" )
    , ( 'z', "#39A78E" ) -- Zomp
    ]
        |> Dict.fromList


color : Char -> ( String, JE.Value )
color char =
    ( "itemStyle"
    , JE.object
        [ ( "color"
          , colors
                |> Dict.get (toLower char)
                |> Maybe.withDefault "#000000"
                |> JE.string
          )
        ]
    )


getColor : Int -> Char
getColor i =
    "*abcdefghijklmnopqrstuvwxyz+ABCDEFGHIJKLMNOPQRSTUVWXYZ#1234567890!§$%&/()=?'_.,;:<>|^°~"
        |> String.slice i -1
        |> String.uncons
        |> Maybe.map Tuple.first
        |> Maybe.withDefault '~'
