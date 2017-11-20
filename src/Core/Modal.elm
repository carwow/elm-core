module Core.Modal exposing (init, update, view, subscriptions)

{-| Modal component


# Exported

@docs init, update, view, subscriptions

-}

import CarwowTheme.Modal exposing (..)
import Html exposing (..)
import Core.ModalPorts exposing (fixScroll)


{-| A function which initialises the modal
-}
init : String -> CarwowTheme.Modal.Model
init id =
    CarwowTheme.Modal.init id


{-| A function to update the modal according to messages received
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchModal isOpen ->
            let
                ( newModel, cmd ) =
                    CarwowTheme.Modal.update (SwitchModal isOpen) model
            in
                ( newModel, fixScroll isOpen )

        message ->
            CarwowTheme.Modal.update message model


{-| The view for the modal component
-}
view : Model -> msg -> msg -> ModalProperties msg -> Html msg
view model openModalEvent closeModalEvent properties =
    CarwowTheme.Modal.view model openModalEvent closeModalEvent properties


{-| A function to set the subscriptions of the component
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    CarwowTheme.Modal.subscriptions model
