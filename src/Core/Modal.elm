module Core.Modal exposing (init, update, view, subscriptions)

{-| Modal component


# Exported

@docs init, update, view, subscriptions

-}

import CarwowTheme.Modal exposing (..)
import Html exposing (..)
import Core.ModalPorts exposing (fixScroll)


{-| Placeholder
-}
init : String -> CarwowTheme.Modal.Model
init id =
    CarwowTheme.Modal.init id


{-| Placeholder
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


{-| Placeholder
-}
view : Model -> msg -> msg -> ModalProperties msg -> Html msg
view model openModalEvent closeModalEvent properties =
    CarwowTheme.Modal.view model openModalEvent closeModalEvent properties


{-| Placeholder
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    CarwowTheme.Modal.subscriptions model
