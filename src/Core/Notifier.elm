module Core.Notifier exposing (subscriptions, view, Model, update, Msg(NewNotification), init, Flags)

{-| Displays latest notifications


# Exported

@docs subscriptions, view, Model, update, Msg, init, Flags

-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, property)
import Http exposing (request)
import CarwowTheme.Drawer as Drawer exposing (Model, Properties, Msg(Toggle), init, view)
import RemoteData exposing (RemoteData, WebData, sendRequest, update)
import Json.Encode as Encode


{-| Placeholder
-}
type alias Model =
    { flags : Flags
    , notifications : WebData String
    , drawer : Drawer.Model
    }


{-| Placeholder
-}
type Msg
    = NewNotification String
    | NotificationsResponse (WebData String)
    | DrawerMsg Drawer.Msg


{-| Placeholder
-}
type alias Flags =
    { notifierApiEndpoint : String
    }


{-| Placeholder
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        drawer =
            Drawer.init "notifications-drawer"

        model =
            Model flags RemoteData.NotAsked drawer
    in
        ( model, (getNotifications model.flags.notifierApiEndpoint) )


{-| Placeholder
-}
view : Model -> Html Msg
view model =
    let
        title =
            "Notifications"

        body =
            case model.notifications of
                RemoteData.Success responseBody ->
                    notificationsView responseBody

                _ ->
                    spinnerView

        drawerProperties =
            Drawer.Properties body title

        toggleMsg =
            DrawerMsg Drawer.Toggle
    in
        Drawer.view model.drawer drawerProperties toggleMsg


notificationsView : String -> Html Msg
notificationsView notifications =
    div [ property "innerHTML" (Encode.string notifications) ] []


spinnerView : Html Msg
spinnerView =
    div [ class "carwow-spinner carwow-spinner-centered" ] []


{-| Placeholder
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        NewNotification string ->
            ( model, (getNotifications model.flags.notifierApiEndpoint) )

        NotificationsResponse response ->
            ( { model | notifications = response }, Cmd.none )

        DrawerMsg message ->
            let
                ( newDrawer, newCmd ) =
                    Drawer.update message model.drawer
            in
                ( { model | drawer = newDrawer }, Cmd.none )


getNotifications : String -> Cmd Msg
getNotifications endpoint =
    Http.request
        { method = "GET"
        , headers = []
        , url = (endpoint ++ "/notifications.html")
        , body = Http.emptyBody
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = True
        }
        |> RemoteData.sendRequest
        |> Cmd.map NotificationsResponse


{-| Placeholder
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DrawerMsg <| Drawer.subscriptions model.drawer
