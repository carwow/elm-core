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
import Json.Encode as Encode
import Dict


type alias PaginatedResponse =
    { totalPages : Int
    , body : String
    }


{-| A representation of the NotificationDrawer
-}
type alias Model =
    { flags : Flags
    , notifications : PaginatedResponse
    , currentPage : Int
    , drawer : Drawer.Model
    , loadMoreDisabled : Bool
    }


{-| The different messages the Notification Drawer accepts

NewNotification — A new notification has occured
NotificationsResponse — A response has been received from the external notifications API
DrawerMsg — An action has occured on the drawer element
LoadMoreNotifications — The user has requested more notifications

-}
type Msg
    = NewNotification String
    | NotificationsResponse (Result Http.Error PaginatedResponse)
    | DrawerMsg Drawer.Msg
    | LoadMoreNotifications


{-| A flag to represent the API endpoint to retreive notifications
-}
type alias Flags =
    { notifierApiEndpoint : String
    }


{-| A representation of an invalid PaginatedResponse
-}
invalidPaginatedResponse : PaginatedResponse
invalidPaginatedResponse =
    PaginatedResponse -1 ""


{-| Initialise the model
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        drawer =
            Drawer.init "notifications-drawer"

        model =
            Model flags invalidPaginatedResponse 1 drawer False
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
            case model.notifications.body of
                "" ->
                    spinnerView

                _ ->
                    notificationsView model.notifications.body

        drawerProperties =
            Drawer.Properties body title

        toggleOpenMsg =
            DrawerMsg (Drawer.Toggle Drawer.Open)

        toggleCloseMsg =
            DrawerMsg (Drawer.Toggle Drawer.Close)
    in
        Drawer.view model.drawer drawerProperties toggleOpenMsg toggleCloseMsg (loadMoreButton model)


{-| Load More button, if there is data to load
-}
loadMoreButton : Model -> Html Msg
loadMoreButton model =
    if model.loadMoreDisabled then
        text ""
    else
        div
            [ class "notification-drawer__load_more"
            ]
            [ button [ class "btn btn-secondary btn-short", onClick LoadMoreNotifications ] [ text "Load More" ] ]


{-| A view representing a spinner, carwow style!
-}
spinnerView : Html Msg
spinnerView =
    div [ class "carwow-spinner carwow-spinner-centered" ] []


{-| Inserts the result of the notification response into the drawer
-}
notificationsView : String -> Html Msg
notificationsView notifications =
    div [ property "innerHTML" (Encode.string notifications) ] []


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
        LoadMoreNotifications ->
            ( { model | currentPage = (model.currentPage + 1) }, fetchResults model.flags.notifierApiEndpoint (model.currentPage + 1) )

        NewNotification string ->
            ( model, (fetchResults model.flags.notifierApiEndpoint model.currentPage) )

        NotificationsResponse response ->
            let
                responseData =
                    case response of
                        Ok paginatedResult ->
                            paginatedResult

                        _ ->
                            invalidPaginatedResponse

                appendedResponse =
                    if model.loadMoreDisabled then
                        responseData.body
                    else
                        model.notifications.body ++ responseData.body

                loadMoreDisabled =
                    model.currentPage >= responseData.totalPages
            in
                ( { model | notifications = PaginatedResponse responseData.totalPages appendedResponse, loadMoreDisabled = loadMoreDisabled }, Cmd.none )

        DrawerMsg message ->
            let
                ( newDrawer, drawerCmd ) =
                    Drawer.update message model.drawer

                drawerOpen =
                    case message of
                        Drawer.Toggle Drawer.Open ->
                            True

                        _ ->
                            False

                newPage =
                    if drawerOpen == True then
                        model.currentPage
                    else
                        1

                notifierCmd =
                    fetchResults model.flags.notifierApiEndpoint model.currentPage

                newModel =
                    { model | drawer = newDrawer, currentPage = newPage }
            in
                ( newModel
                , Cmd.batch
                    [ Cmd.map DrawerMsg drawerCmd
                    , notifierCmd
                    ]
                )


{-| An Http.Expect object that is transformed to a PaginatedResponse
-}
expectPaginatedResults : Http.Expect PaginatedResponse
expectPaginatedResults =
    Http.expectStringResponse
        (\response ->
            Ok
                (PaginatedResponse
                    (response.headers
                        |> Dict.toList
                        |> List.map (Tuple.mapFirst String.toLower)
                        |> Dict.fromList
                        |> Dict.get "x-total-pages"
                        |> Maybe.withDefault "0"
                        |> String.toInt
                        |> Result.toMaybe
                        |> Maybe.withDefault 0
                    )
                    response.body
                )
        )


{-| The representation of the request for Paginated notifications

Note: we request the HTML by default

-}
getNotifications : String -> Int -> Http.Request PaginatedResponse
getNotifications endpoint page =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "text/html" ]
        , url = (endpoint ++ "/notifications?page=" ++ (toString page))
        , body = Http.emptyBody
        , expect = expectPaginatedResults
        , timeout = Nothing
        , withCredentials = True
        }


{-| Make the HTTP request
-}
fetchResults : String -> Int -> Cmd Msg
fetchResults endpoint page =
    Http.send NotificationsResponse (getNotifications endpoint page)


{-| Placeholder
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DrawerMsg <| Drawer.subscriptions model.drawer
