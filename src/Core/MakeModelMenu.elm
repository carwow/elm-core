module Core.MakeModelMenu exposing (processRemoteData, getAvailableMakes, getAvailableModels, modelsApiUrl, makesApiUrl, main, init, update, subscriptions, view, Flags, Model, Msg)

{-| Make/Model menu component


# Exported

@docs processRemoteData, getAvailableMakes, getAvailableModels, modelsApiUrl, makesApiUrl, main, init, update, subscriptions, view, Flags, Model, Msg

-}

import CarwowTheme.Icons exposing (icon)
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
import Regex exposing (..)


{-| Placeholder
-}
main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


{-| Placeholder
-}
type State
    = MakeSelection (WebData (List Core.Data.Make.Make))
    | ModelSelection Core.Data.Make.Make (WebData (List Core.Data.Model.Model))


{-| Placeholder
-}
type alias Model =
    { state : State
    , modal : CarwowTheme.Modal.Model
    , apiEndpointUrl : Erl.Url
    , apiFilterField : String
    , baseLinkUrl : Erl.Url
    , location: String
    }


{-| Placeholder
-}
type Msg
    = MakeSelected Core.Data.Make.Make
    | ModalBackClicked
    | MakesResponse (WebData (List Core.Data.Make.Make))
    | ModelsResponse (WebData (List Core.Data.Model.Model))
    | ModalMsg CarwowTheme.Modal.Msg


{-| Placeholder
-}
type alias Flags =
    { id : String
    , apiEndpointUrl : String
    , apiFilterField : String
    , baseLinkUrl : String
    , location: String
    }


{-| Placeholder
-}
type alias MenuItem a =
    { a | name : String }


{-| Placeholder
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
            Model state modal apiEndpointUrl flags.apiFilterField baseLinkUrl flags.location
    in
        ( model, Cmd.none )


{-| Placeholder
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


{-| Placeholder
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


{-| Placeholder
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


{-| Placeholder
-}
modalMakesView : WebData (List Make) -> Html Msg
modalMakesView makesRemoteData =
    let
        makeView =
            (\make ->
                [ span [ class "makes-models-menu__link makes-models-menu__make", Html.Events.onClick (MakeSelected make) ]
                    [ icon ("makes/" ++ String.join "_" (String.split "-" (String.toLower make.slug))) { size = "large", colour = "grey", colouring = "filled" }
                    , div [ class "makes-models-menu__make" ] [ text make.name ]
                    ]
                ]
            )
    in
        listView makeView makesRemoteData


{-| Placeholder
-}
modalModelsView : Core.Data.Make.Make -> String -> Erl.Url -> WebData (List Core.Data.Model.Model) -> Html Msg
modalModelsView make location baseLinkUrl modelsRemoteData =
    let
        makeModelUrl =
            (\model ->
                location
                    |> Regex.replace All (Regex.regex "www.") (\_ -> "quotes.")
                    -- |> (\from to str -> String.split from str |> String.join to) "www." "quotes."
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


{-| Placeholder
-}
spinnerView : Html msg
spinnerView =
    div
        [ class "makes-models-menu__spinner" ]
        [ div [ class "carwow-spinner carwow-spinner-centered" ] [] ]


{-| Placeholder
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
                    ( modalModelsView make model.location model.baseLinkUrl availableModels
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


{-| Placeholder
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ModalMsg <| Core.Modal.subscriptions model.modal


{-| Placeholder
-}
makesApiUrl : Url -> Url
makesApiUrl apiEndpointUrl =
    apiEndpointUrl
        |> Erl.appendPathSegments [ "api", "v2", "makes" ]


{-| Placeholder
-}
modelsApiUrl : Url -> String -> Url
modelsApiUrl apiEndpointUrl makeSlug =
    makesApiUrl apiEndpointUrl
        |> Erl.appendPathSegments [ makeSlug, "models" ]


{-| Placeholder
-}
getAvailableMakes : Erl.Url -> Cmd Msg
getAvailableMakes availableMakesUrl =
    Http.get (Erl.toString availableMakesUrl) Core.Data.Make.listDecoder
        |> RemoteData.sendRequest
        |> Cmd.map MakesResponse


{-| Placeholder
-}
getAvailableModels : Erl.Url -> Cmd Msg
getAvailableModels availableModelsUrl =
    Http.get (Erl.toString availableModelsUrl) Core.Data.Model.listDecoder
        |> RemoteData.sendRequest
        |> Cmd.map ModelsResponse
