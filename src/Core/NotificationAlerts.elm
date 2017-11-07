module Core.NotificationAlerts exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import CarwowTheme.Icons exposing (icon)
import Json.Decode exposing (..)


type alias Model =
    { unreadCount : Maybe Int }


type Msg
    = UpdateCounter Json.Decode.Value


init : ( Model, Cmd Msg )
init =
    ( (Model Nothing), Cmd.none )


unreadDecoder : Decoder Int
unreadDecoder =
    field "unread_count" int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCounter pushedData ->
            let
                result =
                    decodeValue unreadDecoder pushedData
            in
                case result of
                    Ok count ->
                        ( { model | unreadCount = Just count }, Cmd.none )

                    Err _ ->
                        ( model, Cmd.none )


view : Model -> Html Msg
view model =
    label [ Svg.Attributes.class "main-header-menu-link main-header-menu-link--extra-padded", for "notifications-drawer-open" ]
        [ badgeView model.unreadCount
        , i [ Html.Attributes.class "icon" ] [ icon "bell" { size = "small", colour = "white", colouring = "outline" } ]
        ]


badgeView : Maybe Int -> Html Msg
badgeView unreadCount =
    case unreadCount of
        Just count ->
            span
                [ Svg.Attributes.class "badge badge--regular badge-notification badge--small main-header-menu__badge" ]
                [ Svg.text (toString count) ]

        Nothing ->
            Html.text ""
