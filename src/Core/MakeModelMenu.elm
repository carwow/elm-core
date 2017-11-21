module Core.MakeModelMenu exposing (init, update, subscriptions, view, Flags, Model, Msg)

{-| Make/Model menu component


# Exported

@docs init, update, subscriptions, view, Flags, Model, Msg

-}

import CarwowTheme.Icons exposing (icon, makeIcon)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Core.Modal exposing (..)
import CarwowTheme.Modal exposing (ModalProperties, PaddingStyle)
import RemoteData exposing (RemoteData, WebData, sendRequest, update)
import Http exposing (get)
import Erl exposing (..)
import Core.Data.Make exposing (..)
import Core.Data.Model exposing (..)


{-| The state of the model
-}
type State
    = MakeSelection (WebData (List Core.Data.Make.Make))
    | ModelSelection Core.Data.Make.Make (WebData (List Core.Data.Model.Model))


{-| A representation of the MakeModelMenu

modal - The modal component that the MakeModelMenu will use
apiEndpointUrl - The url of the API endpoint to retrieve the Make/Model data from
apiFilterField - The filter field used in the API request
baseLinkUrl - The path to re-direct to once the Make Model has been selected
redirectLocation - The location that the Menu should re-direct to once the Make & Model has been selected, e.g https://quotes.carwow.co.uk/
-}
type alias Model =
    { state : State
    , modal : CarwowTheme.Modal.Model
    , apiEndpointUrl : Erl.Url
    , apiFilterField : String
    , baseLinkUrl : Erl.Url
    , redirectLocation: String
    }


{-| The different messages the Make Model Menu accepts

ModalBackClicked - The back button is clicked by the user
MakesResponse - A response is received from the Makes endpoint
ModelsResponse -A response is received from the Models endpoint
ModalMsg - A message is received by the underlying Modal component

-}
type Msg
    = MakeSelected Core.Data.Make.Make
    | ModalBackClicked
    | MakesResponse (WebData (List Core.Data.Make.Make))
    | ModelsResponse (WebData (List Core.Data.Model.Model))
    | ModalMsg CarwowTheme.Modal.Msg


{-| Flags which are used to retrieve and display the Make & Model data
-}
type alias Flags =
    { id : String
    , apiEndpointUrl : String
    , apiFilterField : String
    , baseLinkUrl : String
    , redirectLocation: String
    }


{-| A representation of a Make or Model in the menu
-}
type alias MenuItem a =
    { a | name : String }


{-| Initialise the model
-}
init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        modal =
            Core.Modal.init flags.id

        apiEndpointUrl =
            Erl.parse flags.apiEndpointUrl

        baseLinkUrl =
            Erl.parse flags.baseLinkUrl

        state =
            MakeSelection RemoteData.Loading

        model =
            Model state modal apiEndpointUrl flags.apiFilterField baseLinkUrl flags.redirectLocation
    in
        ( model, Cmd.none )


{-| Update the component if a message is received
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModalMsg inner ->
            let
                ( newModal, modalUpdateCmd ) =
                    Core.Modal.update inner model.modal

                ( newModel, makeModelMenuCmd ) =
                    case inner of
                        CarwowTheme.Modal.SwitchModal True ->
                            ( { model | state = (MakeSelection RemoteData.Loading) }, getAvailableMakes (makesApiUrl model.apiEndpointUrl) )

                        _ ->
                            ( model, Cmd.none )
            in
                ( { newModel | modal = newModal }, Cmd.batch [ Cmd.map ModalMsg modalUpdateCmd, makeModelMenuCmd ] )

        MakeSelected make ->
            let
                newState =
                    ModelSelection make RemoteData.Loading

                newCmd =
                    getAvailableModels (modelsApiUrl model.apiEndpointUrl make.slug)
            in
                ( { model | state = newState }
                , newCmd
                )

        MakesResponse response ->
            let
                ( sortedFilteredResponse, _ ) =
                    case model.apiFilterField of
                        "FactoryOrder" ->
                            RemoteData.update (processRemoteData .factoryOrderAvailable) response

                        "Stock" ->
                            RemoteData.update (processRemoteData .stockAvailable) response

                        "LeaseDeals" ->
                            RemoteData.update (processRemoteData .leaseDealsAvailable) response

                        _ ->
                            ( RemoteData.Loading, Cmd.none )
            in
                ( { model | state = MakeSelection sortedFilteredResponse }, Cmd.none )

        ModelsResponse response ->
            let
                ( sortedFilteredResponse, _ ) =
                    case model.apiFilterField of
                        "FactoryOrder" ->
                            RemoteData.update (processRemoteData .factoryOrderAvailable) response

                        "Stock" ->
                            RemoteData.update (processRemoteData .stockAvailable) response

                        "LeaseDeals" ->
                            RemoteData.update (processRemoteData .leaseDealsAvailable) response

                        _ ->
                            ( RemoteData.Loading, Cmd.none )

                newState =
                    case model.state of
                        ModelSelection make _ ->
                            ModelSelection make sortedFilteredResponse

                        _ ->
                            model.state
            in
                ( { model | state = newState }, Cmd.none )

        ModalBackClicked ->
            let
                newState =
                    MakeSelection RemoteData.Loading
            in
                ( { model | state = newState }, getAvailableMakes (makesApiUrl model.apiEndpointUrl) )


{-| Process the Make/Model data retrieved from research site
-}
processRemoteData :
    (MenuItem a -> Bool)
    -> List (MenuItem a)
    -> ( List (MenuItem a), Cmd msg )
processRemoteData field items =
    let
        sortField item =
            item.name |> String.toUpper
    in
        ( items |> List.filter field |> List.sortBy sortField, Cmd.none )


{-| A view representing the list of Makes/Models
-}
listView : (a -> List (Html msg)) -> WebData (List a) -> Html msg
listView itemView itemsRemoteData =
    case itemsRemoteData of
        RemoteData.Success items ->
            ul [ class "makes-models-menu__list list-unstyled" ]
                (items
                    |> List.map
                        (\item ->
                            li
                                [ class "makes-models-menu__list-item" ]
                                (itemView item)
                        )
                )

        _ ->
            spinnerView


{-| A view representing a singular Make
-}
modalMakesView : WebData (List Make) -> Html Msg
modalMakesView makesRemoteData =
    let
        makeView =
            (\make ->
                [ span [ class "makes-models-menu__link makes-models-menu__make", Html.Events.onClick (MakeSelected make) ]
                    [ makeIcon (String.toLower make.slug) Nothing, div [ class "makes-models-menu__make" ] [ text make.name ] ]
                ]
            )
    in
        listView makeView makesRemoteData


{-| A view representing a singular Model
-}
modalModelsView : Core.Data.Make.Make -> String -> Erl.Url -> WebData (List Core.Data.Model.Model) -> Html Msg
modalModelsView make redirectLocation baseLinkUrl modelsRemoteData =
    let
        makeModelUrl =
            (\model ->
                redirectLocation
                    |> Erl.parse
                    |> Erl.appendPathSegments (Erl.toString(baseLinkUrl) |> String.split "/")
                    |> Erl.addQuery "make" make.slug
                    |> Erl.addQuery "model" model.slug
                    |> Erl.toString
            )

        modelView =
            (\model ->
                [ a [ href (makeModelUrl model), class "makes-models-menu__link makes-models-menu__model" ]
                    [ text model.name ]
                ]
            )
    in
        listView modelView modelsRemoteData


{-| A view representing a spinner
-}
spinnerView : Html msg
spinnerView =
    div
        [ class "makes-models-menu__spinner" ]
        [ div [ class "carwow-spinner carwow-spinner-centered" ] [] ]


{-| A view representing the whole model
-}
view : Model -> Html Msg
view model =
    let
        ( body, title, backButton ) =
            case model.state of
                MakeSelection availableMakes ->
                    ( modalMakesView availableMakes
                    , "Choose make"
                    , div [] []
                    )

                ModelSelection make availableModels ->
                    ( modalModelsView make model.redirectLocation model.baseLinkUrl availableModels
                    , "Choose model"
                    , div
                        [ Html.Attributes.class "modal__header-button"
                        , Html.Events.onClick ModalBackClicked
                        ]
                        [ icon "caret_left" { size = "small", colour = "dark-grey", colouring = "outline" } ]
                    )

        modalProperties =
            ModalProperties body title CarwowTheme.Modal.NoPadding Nothing (Just backButton)

        openModalMsg =
            ModalMsg (CarwowTheme.Modal.SwitchModal True)

        closeModalMsg =
            ModalMsg (CarwowTheme.Modal.SwitchModal False)
    in
        Core.Modal.view model.modal openModalMsg closeModalMsg modalProperties


{-| The subscriptions of this model
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ModalMsg <| Core.Modal.subscriptions model.modal


{-| A function which returns the path to the Makes endpoint
-}
makesApiUrl : Url -> Url
makesApiUrl apiEndpointUrl =
    apiEndpointUrl
        |> Erl.appendPathSegments [ "api", "v2", "makes" ]


{-| A function which returns the path to the Models endpoint
-}
modelsApiUrl : Url -> String -> Url
modelsApiUrl apiEndpointUrl makeSlug =
    makesApiUrl apiEndpointUrl
        |> Erl.appendPathSegments [ makeSlug, "models" ]


{-| A function which returns the list of available Makes from the API
-}
getAvailableMakes : Erl.Url -> Cmd Msg
getAvailableMakes availableMakesUrl =
    Http.get (Erl.toString availableMakesUrl) Core.Data.Make.listDecoder
        |> RemoteData.sendRequest
        |> Cmd.map MakesResponse


{-| A function which returns the list of available Models from the API
-}
getAvailableModels : Erl.Url -> Cmd Msg
getAvailableModels availableModelsUrl =
    Http.get (Erl.toString availableModelsUrl) Core.Data.Model.listDecoder
        |> RemoteData.sendRequest
        |> Cmd.map ModelsResponse
