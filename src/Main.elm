module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes
import List exposing (maximum, minimum)
import Random exposing (Generator)
import RemoteData exposing (RemoteData(..), WebData)
import String exposing (fromFloat)
import Svg exposing (Svg, g, line, rect, style, svg, text_)
import Svg.Attributes exposing (fontSize, height, stroke, strokeDasharray, transform, viewBox, width, x, x1, x2, y, y1, y2)
import Svg.Attributes exposing (fill)


type alias Model =
    { remoteNumbers : WebData (List Int) }


randomIntList : Generator (List Int)
randomIntList =
    Random.list 200 (Random.int 1 10)


getList : Cmd Msg
getList =
    Random.generate GotList randomIntList


initialModel : Model
initialModel =
    { remoteNumbers = RemoteData.succeed [ 0 ] }


type Msg
    = GotList (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotList list ->
            ( { model
                | remoteNumbers =
                    RemoteData.succeed list
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "container" ]
        [ viewRandomList model.remoteNumbers ]


viewRandomList : WebData (List Int) -> Html Msg
viewRandomList data =
    svg [ viewBox "0 0 1000 1000" ]
        (case data of
            Success list ->
                viewGraph list

            _ ->
                []
        )


viewGraph : List Int -> List (Svg Msg)
viewGraph nums =
    let
        xAxis =
            line
                [ x1 "5%"
                , y1 "95%"
                , x2 "5%"
                , y2 "0%"
                , stroke "gray"
                ]
                []

        yAxis =
            line
                [ x1 "5%"
                , y1 "95%"
                , x2 "100%"
                , y2 "95%"
                , stroke "gray"
                ]
                []

        yTicks tick ticks maxValue =
            let
                tickHeight =
                    (95 / ticks * tick)
                        |> round
                        |> String.fromInt

                tickValue =
                    maxValue
                        - (maxValue / ticks * tick)
                        |> round
                        |> String.fromInt

                yTickLabel =
                    text_
                        [ x "2%"
                        , y (tickHeight ++ "%")
                        , fontSize "20px"
                        , Svg.Attributes.class "tick-label"
                        ]
                        [ style
                            []
                            [ text ".tick-label {fill: white}"
                            ]
                        , text tickValue
                        ]

                yDashedLine =
                    line
                        [ stroke "gray"
                        , strokeDasharray "3,3"
                        , y1 <|
                            tickHeight
                                ++ "%"
                        , y2 <|
                            tickHeight
                                ++ "%"
                        , x1 "7%"
                        , x2 "100%"
                        ]
                        []

                yTickMark =
                    line
                        [ x1 "5%"
                        , x2 "7%"
                        , y1 <|
                            tickHeight
                                ++ "%"
                        , y2 <|
                            tickHeight
                                ++ "%"
                        , stroke "gray"
                        ]
                        []
            in
            g []
                [ yTickMark
                , yTickLabel
                , yDashedLine
                ]

        viewBins : Dict Int Int -> List (Svg Msg)
        viewBins binDict =
            let
                binKeys =
                    Dict.keys binDict

                minBin =
                    minimum binKeys
                        |> Maybe.withDefault 1
                        |> toFloat

                maxBin =
                    maximum binKeys
                        |> Maybe.withDefault 10
                        |> toFloat

                barWidth =
                    String.fromFloat ((95.0 / (maxBin - minBin)) - 5) ++ "%"

                barHeight val =
                    val
                        * 95
                        / 40.5
                        |> fromFloat
                        |> (\s -> String.append s "%")

                barStart val =
                    (val * (maxBin - minBin) * 95/100) - 5
                        |> fromFloat
                        |> (\s -> String.append s "%")

                viewBar ix val =
                    rect
                        [ x <| barStart ix
                        , y "0%"
                        , width barWidth
                        , height <| barHeight val
                        , transform "translate(50 950) scale(1,-1)"
                        , fill "green"
                        ]
                        []
            in
            Dict.toList binDict
                |> List.map (\( k, v ) -> viewBar (toFloat k) (toFloat v))

        bins : Dict Int Int
        bins =
            List.foldl
                (\k dict ->
                    case Dict.get k dict of
                        Just v ->
                            Dict.insert k (v + 1) dict

                        Nothing ->
                            Dict.insert k 1 dict
                )
                Dict.empty
                nums
    in
    List.append
        [ xAxis
        , yAxis
        , yTicks 1 4 40
        , yTicks 2 4 40
        , yTicks 3 4 40
        ]
        (viewBins
            bins
        )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( initialModel, getList )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
