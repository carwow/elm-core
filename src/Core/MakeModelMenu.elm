module Core.MakeModelMenu exposing (init, update, subscriptions, view, Flags, Model, Msg(UrlChange))

{-| Make/Model menu component


# Exported

@docs init, update, subscriptions, view, Flags, Model, Msg

-}

import CarwowTheme.Icons exposing (icon, makeIcon)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import CarwowTheme.Modal exposing (..)
import RemoteData exposing (RemoteData, WebData, sendRequest, update)
import Http exposing (get)
import Erl exposing (..)
import Core.Data.Make exposing (..)
import Core.Data.Model exposing (..)
import Navigation


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
redirectUrl - The url that the Menu should re-direct to once the Make & Model has been selected, e.g <https://quotes.carwow.co.uk/>

-}
type alias Model =
    { state : State
    , modal : CarwowTheme.Modal.Model
    , apiEndpointUrl : Erl.Url
    , apiFilterField : String
    , baseLinkUrl : Erl.Url
    , redirectUrl : String
    , preselectedMakeSlug : Maybe String
    , location : Url
    }


{-| The different messages the Make Model Menu accepts

ModalBackClicked - The back button is clicked by the user
MakesResponse - A response is received from the Makes endpoint
ModelsResponse -A response is received from the Models endpoint
ModalMsg - A message is received by the underlying Modal component
UrlChange - A message is received when the address bar changes

-}
type Msg
    = MakeSelected Core.Data.Make.Make
    | ModalBackClicked
    | MakesResponse (WebData (List Core.Data.Make.Make))
    | ModelsResponse (WebData (List Core.Data.Model.Model))
    | ModalMsg CarwowTheme.Modal.Msg
    | UrlChange Navigation.Location


{-| Flags which are used to retrieve and display the Make & Model data
-}
type alias Flags =
    { id : String
    , apiEndpointUrl : String
    , apiFilterField : String
    , baseLinkUrl : String
    , redirectUrl : String
    }


{-| A representation of a Make or Model in the menu
-}
type alias MenuItem a =
    { a | name : String }


{-| A function which maps the address bar hash link to the modal open state
-}
mapUrlToModalOpen : Erl.Url -> Bool
mapUrlToModalOpen url =
    url.hash == "make-model-menu"


{-| Initialise the model
-}
init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        url =
            Erl.parse location.href

        modal =
            CarwowTheme.Modal.init flags.id (mapUrlToModalOpen url)

        apiEndpointUrl =
            Erl.parse flags.apiEndpointUrl

        baseLinkUrl =
            Erl.parse flags.baseLinkUrl

        state =
            MakeSelection RemoteData.Loading

        preselectedMakeSlug =
            Erl.getQueryValuesForKey "make" url |> List.head

        model =
            Model state modal apiEndpointUrl flags.apiFilterField baseLinkUrl flags.redirectUrl preselectedMakeSlug url

        getMakesCmd =
            case mapUrlToModalOpen url of
                True ->
                    getAvailableMakes (makesApiUrl model.apiEndpointUrl)

                False ->
                    Cmd.none

        commands =
            Cmd.batch [ getMakesCmd ]
    in
        ( model, commands )


{-| Update the component if a message is received
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModalMsg inner ->
            let
                ( newModal, modalUpdateCmd ) =
                    CarwowTheme.Modal.update inner model.modal

                navigationCmd =
                    if newModal.isOpen == False then
                        locationWithoutHash (model.location)
                            |> locationWithoutMake
                            |> Erl.toString
                            |> Navigation.newUrl
                    else
                        Cmd.none

                ( newModel, makeModelMenuCmd ) =
                    case inner of
                        CarwowTheme.Modal.SwitchModal True ->
                            ( { model | state = (MakeSelection RemoteData.Loading) }, getAvailableMakes (makesApiUrl model.apiEndpointUrl) )

                        _ ->
                            ( model, Cmd.none )
            in
                ( { newModel | modal = newModal }, Cmd.batch [ Cmd.map ModalMsg modalUpdateCmd, makeModelMenuCmd, navigationCmd ] )

        MakeSelected make ->
            let
                newState =
                    ModelSelection make RemoteData.Loading

                makesUrl =
                    makesApiUrl model.apiEndpointUrl

                newCmd =
                    getAvailableModels (modelsApiUrl makesUrl make.slug)
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

                make =
                    findPreselectedMake model.preselectedMakeSlug sortedFilteredResponse

                newModel =
                    { model | state = MakeSelection sortedFilteredResponse }
            in
                Maybe.map2 (\preselected make -> (update (MakeSelected make) newModel)) model.preselectedMakeSlug make
                    |> Maybe.withDefault ( newModel, Cmd.none )

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

                commands =
                    Cmd.batch
                        [ getAvailableMakes (makesApiUrl model.apiEndpointUrl)
                        , locationWithoutMake (model.location) |> Erl.toString |> Navigation.newUrl
                        ]
            in
                ( { model | state = newState, preselectedMakeSlug = Nothing }, commands )

        UrlChange location ->
            let
                ( newModal, modalUpdateCmd ) =
                    case mapUrlToModalOpen (Erl.parse location.href) of
                        True ->
                            CarwowTheme.Modal.update (CarwowTheme.Modal.SwitchModal True) model.modal

                        _ ->
                            ( model.modal, Cmd.none )

                ( newModel, makeModelMenuCmd ) =
                    case mapUrlToModalOpen (Erl.parse location.href) of
                        True ->
                            ( { model | state = (MakeSelection RemoteData.Loading) }, getAvailableMakes (makesApiUrl model.apiEndpointUrl) )

                        _ ->
                            ( model, Cmd.none )

                preselectedMakeSlug =
                    Erl.parse location.href |> Erl.getQueryValuesForKey "make" |> List.head
            in
                ( { newModel | modal = newModal, location = (Erl.parse location.href), preselectedMakeSlug = preselectedMakeSlug }, Cmd.batch [ Cmd.map ModalMsg modalUpdateCmd, makeModelMenuCmd ] )


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


{-| Find the preselected make based on matching slug against make.slug
-}
findPreselectedMake : Maybe String -> WebData (List Make) -> Maybe Make
findPreselectedMake slug makes =
    case slug of
        Nothing ->
            Nothing

        Just slug ->
            makes
                |> RemoteData.toMaybe
                |> Maybe.andThen (List.filter (\x -> x.slug == slug) >> List.head)


locationWithoutMake : Url -> Url
locationWithoutMake url =
    Erl.removeQuery "make" url


locationWithoutHash : Url -> Url
locationWithoutHash url =
    { url | hash = "" }


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
                [ a
                    [ href "javascript:void(0)"
                    , class "makes-models-menu__link makes-models-menu__make"
                    , attribute "data-interaction-element" "Choose make"
                    , attribute "data-interaction-section" "make-models modal"
                    , attribute "data-interaction-type" "select make"
                    , Html.Events.onClick (MakeSelected make)
                    ]
                    [ makeIcon (String.toLower make.slug) Nothing, div [ class "makes-models-menu__make" ] [ text make.name ] ]
                ]
            )
    in
        listView makeView makesRemoteData


{-| A view representing a singular Model
-}
modalModelsView : Core.Data.Make.Make -> String -> Erl.Url -> WebData (List Core.Data.Model.Model) -> Html Msg
modalModelsView make redirectUrl baseLinkUrl modelsRemoteData =
    let
        makeModelUrl =
            (\model ->
                redirectUrl
                    |> Erl.parse
                    |> Erl.appendPathSegments (Erl.toString (baseLinkUrl) |> String.split "/")
                    |> Erl.addQuery "make_slug" make.slug
                    |> Erl.addQuery "model_slug" model.slug
                    |> Erl.toString
            )

        modelView =
            (\model ->
                [ a
                    [ href (makeModelUrl model)
                    , class "makes-models-menu__link makes-models-menu__model"
                    , attribute "data-interaction-element" "Choose model"
                    , attribute "data-interaction-section" "make-models modal"
                    , attribute "data-interaction-type" "select model"
                    ]
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
                    , div [] [ text "Choose make" ]
                    , div [] []
                    )

                ModelSelection make availableModels ->
                    ( modalModelsView make model.redirectUrl model.baseLinkUrl availableModels
                    , div [] [ text "Choose model" ]
                    , div
                        [ Html.Attributes.class "modal__header-button"
                        , Html.Events.onClick ModalBackClicked
                        ]
                        [ icon "caret_left" { size = "small", colour = "dark-grey", colouring = "outline" } ]
                    )

        modalProperties =
            ModalProperties body (Just title) CarwowTheme.Modal.NoPadding Nothing (Just backButton)

        openModalMsg =
            ModalMsg (CarwowTheme.Modal.SwitchModal True)

        closeModalMsg =
            ModalMsg (CarwowTheme.Modal.SwitchModal False)
    in
        CarwowTheme.Modal.view model.modal openModalMsg closeModalMsg modalProperties


{-| The subscriptions of this model
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ModalMsg <| CarwowTheme.Modal.subscriptions model.modal


{-| A function which returns the path to the Makes endpoint
-}
makesApiUrl : Url -> Url
makesApiUrl apiEndpointUrl =
    apiEndpointUrl
        |> Erl.appendPathSegments [ "api", "v2", "makes" ]


{-| A function which returns the path to the Models endpoint
-}
modelsApiUrl : Url -> String -> Url
modelsApiUrl makesApiUrl makeSlug =
    makesApiUrl
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
