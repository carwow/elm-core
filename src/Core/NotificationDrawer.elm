module Core.NotificationDrawer exposing (Msg(NewNotification, DrawerMsg), Model, Flags, subscriptions, view, update, init, isDrawerOpen)

{-| Displays latest notifications


# Exported

@docs Msg, Model, Flags, subscriptions, view, update, init, isDrawerOpen

-}

import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class, property)
import Html.Events exposing (onClick)
import Http exposing (request)
import CarwowTheme.Drawer as Drawer exposing (Model, Properties, Msg(Toggle), init, view, Action(Open, Close))
import RemoteData exposing (RemoteData, WebData, sendRequest, update)
import Json.Encode as Encode


{-| A representation of the NotificationDrawer
-}
type alias Model =
    { flags : Flags
    , notifications : WebData String
    , page : Int
    , drawer : Drawer.Model
    , loadMoreDisabled : Bool
    }


{-| The different messages the Notification Drawer accepts

NewNotification — A new notification has occured
NotificationsResponse — A response has been received from the external notifications API
LoadMoreNotifications — The user has requested more notifications
DrawerMsg — An action has occured on the drawer element

-}
type Msg
    = NewNotification String
    | NotificationsResponse (WebData String)
    | DrawerMsg Drawer.Msg
    | LoadMore


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

        page =
            1

        loadMoreDisabled =
            False

        model =
            Model flags RemoteData.NotAsked page drawer loadMoreDisabled
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
        Drawer.view model.drawer drawerProperties toggleOpenMsg toggleCloseMsg (loadMoreButton model)


{-| Load more button view
-}
loadMoreButton : Model -> Html Msg
loadMoreButton model =
    if model.loadMoreDisabled then
        text ""
    else
        div
            [ class "notification-drawer__load_more"
            ]
            [ button [ class "btn btn-secondary btn-short", onClick LoadMore ] [ text "Load More" ] ]


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


isResponseEmpty : WebData String -> Bool
isResponseEmpty webData =
    (toString webData) == "Success \"\"" || (toString webData) == "Success \"<p class='heading--small-underline heading--small-underline-center'>Nothing new.</p>\\n\""


{-| Update the model based on the message received
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        LoadMore ->
            ( { model | page = (model.page + 1) }, getNotifications model.flags.notifierApiEndpoint (model.page + 1) )

        NewNotification string ->
            ( model, (getNotifications model.flags.notifierApiEndpoint model.page) )

        NotificationsResponse response ->
            let
                appendedString =
                    RemoteData.map2 String.append model.notifications response

                _ =
                    Debug.log "isResponseEmpty" (toString (isResponseEmpty response))

                _ =
                    Debug.log "response" (toString response)
            in
                case model.page of
                    1 ->
                        ( { model | notifications = response, loadMoreDisabled = isResponseEmpty response }, Cmd.none )

                    _ ->
                        ( { model | notifications = RemoteData.map2 String.append model.notifications response, loadMoreDisabled = isResponseEmpty response }, Cmd.none )

        DrawerMsg message ->
            let
                ( newDrawer, drawerCmd ) =
                    Drawer.update message model.drawer

                newModel =
                    { model | drawer = newDrawer }

                notifierCmd =
                    case message of
                        Drawer.Toggle Drawer.Open ->
                            getNotifications model.flags.notifierApiEndpoint model.page

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
getNotifications : String -> Int -> Cmd Msg
getNotifications endpoint page =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "text/html" ]
        , url = (endpoint ++ "/notifications?page=" ++ (toString page))
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
