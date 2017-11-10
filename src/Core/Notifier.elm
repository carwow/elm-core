module Core.Notifier exposing (Msg(NewNotification, DrawerMsg), Model, Flags, subscriptions, view, update, init, isDrawerOpen)

{-| Displays latest notifications


# Exported

@docs Msg, Model, Flags, subscriptions, view, update, init, isDrawerOpen

-}

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, property)
import Http exposing (request)
import CarwowTheme.Drawer as Drawer exposing (Model, Properties, Msg(Toggle), init, view, Action(Open, Close))
import RemoteData exposing (RemoteData, WebData, sendRequest, update)
import Json.Encode as Encode


{-| A representation of the NotificationDrawer
-}
type alias Model =
    { flags : Flags
    , notifications : WebData String
    , drawer : Drawer.Model
    }


{-| The different messages the Notification Drawer accepts

NewNotification — A new notification has occured
NotificationsResponse — A response has been received from the external notifications API
DrawerMsg — An action has occured on the drawer element

-}
type Msg
    = NewNotification String
    | NotificationsResponse (WebData String)
    | DrawerMsg Drawer.Msg


{-| A flag to represent the API endpoint to retreive notifications
-}
type alias Flags =
    { notifierApiEndpoint : String
    }


{-| Initialise the model
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        drawer =
            Drawer.init "notifications-drawer"

        model =
            Model flags RemoteData.NotAsked drawer
    in
        ( model, Cmd.none )


{-| The view for the notification drawer component
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

        toggleOpenMsg =
            DrawerMsg (Drawer.Toggle Drawer.Open)

        toggleCloseMsg =
            DrawerMsg (Drawer.Toggle Drawer.Close)
    in
        Drawer.view model.drawer drawerProperties toggleOpenMsg toggleCloseMsg


{-| Inserts the result of the notification response into the drawer
-}
notificationsView : String -> Html Msg
notificationsView notifications =
    div [ property "innerHTML" (Encode.string notifications) ] []


{-| Render a spinner view
-}
spinnerView : Html Msg
spinnerView =
    div [ class "carwow-spinner carwow-spinner-centered" ] []


{-| The current state of the drawer.

Returns true if the drawer is opened, false if it isn't

-}
isDrawerOpen : Model -> Bool
isDrawerOpen model =
    model.drawer.state == Drawer.Opened


{-| Update the model based on the message received
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
                ( newDrawer, drawerCmd ) =
                    Drawer.update message model.drawer

                newModel =
                    { model | drawer = newDrawer }

                notifierCmd =
                    case message of
                        Drawer.Toggle Drawer.Open ->
                            getNotifications model.flags.notifierApiEndpoint

                        _ ->
                            Cmd.none
            in
                ( newModel
                , Cmd.batch
                    [ Cmd.map DrawerMsg drawerCmd
                    , notifierCmd
                    ]
                )


{-| Make a request to the endpoint representing the notifications

Note: we request the HTML by default

-}
getNotifications : String -> Cmd Msg
getNotifications endpoint =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "text/html" ]
        , url = (endpoint ++ "/notifications")
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
