module Core.PcpCalculator exposing (init, update, subscriptions, view, Model, Msg)

{-| PcpCalculator Component


# Exported

@docs init, update, subscriptions, view, Model, Msg

-}

import Html exposing (Html, Attribute, div, input, text, label, span, program)
import CarwowTheme.Icons exposing (icon)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)
import String
import SingleSlider as SingleSlider exposing (..)


-- Model


{-| The state of the model
-}
type alias Model =
    { carPrice : Int
    , deposit : Int
    , dealerContribution : Int
    , finalPayment : Int
    , financeLength : Int
    , mileageAgreement : Int
    , apr : Float
    , financeLengthSlider : SingleSlider.Model
    , mileageAgreementSlider : SingleSlider.Model
    , aprSlider : SingleSlider.Model
    }


{-| Init the model with default values
-}
init : ( Model, Cmd Msg )
init =
    ( { carPrice = 0
      , deposit = 0
      , dealerContribution = 0
      , finalPayment = 0
      , financeLength = 36
      , mileageAgreement = 10000
      , apr = 7.0
      , financeLengthSlider = defaultFinanceLengthSlider
      , mileageAgreementSlider = defaultMileageAgreementSlider
      , aprSlider = defaultAprSlider
      }
    , Cmd.none
    )


defaultFinanceLengthSlider : SingleSlider.Model
defaultFinanceLengthSlider =
    SingleSlider.init
        { min = 12
        , max = 48
        , step = 6
        , value = 36
        }


defaultMileageAgreementSlider : SingleSlider.Model
defaultMileageAgreementSlider =
    SingleSlider.init
        { min = 5000
        , max = 30000
        , step = 5000
        , value = 10000
        }


defaultAprSlider : SingleSlider.Model
defaultAprSlider =
    SingleSlider.init
        { min = 100
        , max = 700
        , step = 10
        , value = 100
        }



-- Update


{-| Define the messages that the update function can receive
-}
type Msg
    = CarPriceChanged String
    | DepositChanged String
    | DealerContributionChanged String
    | FinalPaymentChanged String
    | FinanceLengthEvent SingleSlider.Msg
    | MileageAgreementEvent SingleSlider.Msg
    | AprEvent SingleSlider.Msg


{-| Update the model based on the message received
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CarPriceChanged amount ->
            let
                newModel =
                    { model | carPrice = (Result.withDefault 0 (String.toInt (amount))) }
            in
                ( newModel, Cmd.none )

        DepositChanged amount ->
            let
                newModel =
                    { model | deposit = (Result.withDefault 0 (String.toInt (amount))) }
            in
                ( newModel, Cmd.none )

        DealerContributionChanged amount ->
            let
                newModel =
                    { model | dealerContribution = (Result.withDefault 0 (String.toInt (amount))) }
            in
                ( newModel, Cmd.none )

        FinalPaymentChanged amount ->
            let
                newModel =
                    { model | finalPayment = (Result.withDefault 0 (String.toInt (amount))) }
            in
                ( newModel, Cmd.none )

        FinanceLengthEvent inner ->
            let
                ( newFinanceLengthSlider, cmd, userInputStopped ) =
                    SingleSlider.update inner model.financeLengthSlider

                newModel =
                    { model
                        | financeLengthSlider = newFinanceLengthSlider
                    }
            in
                ( newModel, Cmd.none )

        MileageAgreementEvent inner ->
            let
                ( newMileageAgreementSlider, cmd, userInputStopped ) =
                    SingleSlider.update inner model.mileageAgreementSlider

                newModel =
                    { model
                        | mileageAgreementSlider = newMileageAgreementSlider
                    }
            in
                ( newModel, Cmd.none )

        AprEvent inner ->
            let
                ( newAprSlider, cmd, userInputStopped ) =
                    SingleSlider.update inner model.aprSlider

                newModel =
                    { model
                        | aprSlider = newAprSlider
                    }
            in
                ( newModel, Cmd.none )



-- Subscriptions


{-| There are no subscriptions but we need to define the function
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map FinanceLengthEvent <|
            SingleSlider.subscriptions model.financeLengthSlider
        , Sub.map MileageAgreementEvent <|
            SingleSlider.subscriptions model.mileageAgreementSlider
        , Sub.map AprEvent <|
            SingleSlider.subscriptions model.aprSlider
        ]



-- View


{-| A view represneting a slider component
-}
viewSlider : Model -> SingleSlider.Model -> String -> (SingleSlider.Msg -> Msg) -> Html Msg
viewSlider model slider labelText sliderMsg =
    div [ class "view-budget-inputs" ]
        [ label [ class "form-input-label", for "input" ]
            [ text labelText ]
        , div [ class "form-input" ]
            [ div [ class "budget-slider-desktop" ]
                [ SingleSlider.view slider |> Html.map sliderMsg ]
            , div [ class "budget-slider-mobile" ]
                [ SingleSlider.view slider |> Html.map sliderMsg ]
            , div [ class "input-range-label input-range-label--current-value" ]
                [ text (toString slider.value) ]
            ]
        ]


inputHtml : (String -> Msg) -> String -> String -> String -> Html Msg
inputHtml updateMsg labelText placeholderText idText =
    div [ class "pcp-calculator-form-row" ]
        [ label [ class "form-input-label" ] [ text labelText ]
        , div [ class "input-infield-container" ] [ input [ type_ "tel", class "input-with-icon", id ("input-infield-icon-" ++ idText), placeholder placeholderText, onInput updateMsg ] [], label [ class "input__infield-icon input--number", for ("input-infield-icon-" ++ idText) ] [ icon "currency_gbp" { size = "small", colour = "dark-grey", colouring = "outline" } ] ]
        ]


{-| A view represneting the model
-}
view : Model -> Html Msg
view model =
    div [ class "pcp-calculator" ]
        [ div [ class "pcp-calculator-form__input" ]
            [ inputHtml CarPriceChanged "Car Price" "i.e. £20,000" "carPrice"
            , inputHtml DepositChanged "Your Deposit" "i.e. £2,500" "customerDeposit"
            , inputHtml DealerContributionChanged "Dealer Contribution" "i.e. £1,000" "dealerContribution"
            , inputHtml FinalPaymentChanged "Final Payment" "i.e. £1,000" "finalPayment"
            ]
        , div [ class "pcp-calculator-form__amount-financed" ]
            [ div [ class "pcp-finance-amount-financed__label" ] [ text " Amount to be financed " ]
            , div [ class "pcp-finance-amount-financed__amount" ] [ text (formatPaymentCurrency (netLoan model)) ]
            ]
        , div [ class "pcp-calculator-form__ranges" ]
            [ viewSlider model model.financeLengthSlider "Finance length" FinanceLengthEvent
            , viewSlider model model.mileageAgreementSlider "Mileage agreement" MileageAgreementEvent
            , viewSlider model model.aprSlider "APR" AprEvent
            ]
        , div [ class "pcp-calculator-form__estimated-monthly-payment" ]
            [ div [ class "pcp-calculator__payment-label" ] [ text "Estimated monthly payment" ]
            , div [ class "pcp-calculator__payment-amount" ] [ text (formatPaymentCurrency (monthlyPayment model)) ]
            ]
        ]



-- Functions


netLoan : Model -> Float
netLoan inputs =
    toFloat (inputs.carPrice - inputs.deposit - inputs.dealerContribution - inputs.finalPayment)


monthlyRate : Model -> Float
monthlyRate inputs =
    (inputs.aprSlider.value / 12) / 100


monthlyPayment : Model -> Float
monthlyPayment inputs =
    let
        monthlyAprRate =
            monthlyRate inputs

        financeAmount =
            netLoan inputs
    in
        (financeAmount * (monthlyAprRate))
            / (1 - ((1 + monthlyAprRate) ^ (-inputs.financeLengthSlider.value)))
            + (toFloat (inputs.finalPayment) * (monthlyAprRate))



-- Helpers


integerUsLocale : Locale
integerUsLocale =
    { usLocale
        | decimals = 0
    }


formatPaymentCurrency : Float -> String
formatPaymentCurrency number =
    "£" ++ format integerUsLocale number
