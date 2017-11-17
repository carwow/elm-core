module Core.NotificationMenu exposing (Msg(UpdateBadge), init, view, update, Model)

{-| Displays notification alerts


# Exported

@docs view, Model, update, Msg, init

-}

import Html exposing (text, span, label, i, Html)
import Html.Attributes exposing (for, attribute)
import Html.Events exposing (onClick)
import Svg exposing (text)
import Svg.Attributes exposing (class)
import CarwowTheme.Icons exposing (icon)
import Json.Decode exposing (int, Decoder, decodeValue, field)


{-| Placeholder
-}
type alias Model =
    { unseenCount : Maybe Int }


{-| Placeholder
-}
type Msg
    = UpdateBadge Json.Decode.Value
    | ClearBadge


{-| Placeholder
-}
init : ( Model, Cmd Msg )
init =
    ( (Model Nothing), Cmd.none )


decoder : Decoder Int
decoder =
    field "unseen_count" int


decodeResponse : Json.Decode.Value -> Result String Int
decodeResponse json =
    decodeValue decoder json


{-| Placeholder
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateBadge json ->
            case decodeResponse json of
                Ok count ->
                    ( { model | unseenCount = Just count }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ClearBadge ->
            ( { model | unseenCount = Nothing }, Cmd.none )


{-| Placeholder
-}
view : Model -> Html Msg
view model =
    let
        unseenCount =
            (toString (Maybe.withDefault 0 model.unseenCount))
    in
        label
            [ class "main-header-menu-link main-header-menu-link--extra-padded"
            , for "notifications-drawer-open"
            , onClick ClearBadge
            , attribute "data-interaction-section" "notification menu"
            , attribute "data-interaction-type" "open modal"
            , attribute "data-unseen-count" unseenCount
            ]
            [ badgeView model.unseenCount
            , i
                [ class "icon icon--notification" ]
                [ icon "bell" { size = "small", colour = "white", colouring = "outline" } ]
            ]


badgeView : Maybe Int -> Html Msg
badgeView unseenCount =
    case unseenCount of
        Just count ->
            span
                [ class "badge badge--regular badge-notification badge--small main-header-menu__badge" ]
                [ Svg.text (toString count) ]

        Nothing ->
            Html.text ""
