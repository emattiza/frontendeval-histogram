module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { remoteNumbers : WebData (List Int) }


initialModel : Model
initialModel =
    { remoteNumbers = NotAsked }


type Msg
    = Increment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view _ =
    div [ class "btn-group" ]
        []


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
