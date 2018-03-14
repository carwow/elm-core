module Core.PcpCalculator exposing (init, update, subscriptions, view, Model, Msg)

{-| PcpCalculator Component


# Exported

@docs init, update, subscriptions, view, Model, Msg

-}

import Html exposing (Html, Attribute, div, input, text, label, span, program)
import CarwowTheme.Icons exposing (icon)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Core.CurrencyFormatter exposing (formatCurrency)
import String


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
      }
    , Cmd.none
    )



-- Update


{-| Define the messages that the update function can receive
-}
type Msg
    = CarPriceChanged String
    | DepositChanged String
    | DealerContributionChanged String
    | FinalPaymentChanged String
    | FinanceLengthChanged String
    | MileageAgreementChanged String
    | AprChanged String


{-| Update the model based on the message received
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CarPriceChanged amount ->
            let
                newModel =
                    { model | carPrice = String.toInt amount |> Result.withDefault 0 }
            in
                ( newModel, Cmd.none )

        DepositChanged amount ->
            let
                newModel =
                    { model | deposit = String.toInt amount |> Result.withDefault 0 }
            in
                ( newModel, Cmd.none )

        DealerContributionChanged amount ->
            let
                newModel =
                    { model | dealerContribution = String.toInt amount |> Result.withDefault 0 }
            in
                ( newModel, Cmd.none )

        FinalPaymentChanged amount ->
            let
                newModel =
                    { model | finalPayment = String.toInt amount |> Result.withDefault 0 }
            in
                ( newModel, Cmd.none )

        FinanceLengthChanged amount ->
            let
                newModel =
                    { model | financeLength = String.toInt amount |> Result.withDefault 0 }
            in
                ( newModel, Cmd.none )

        MileageAgreementChanged amount ->
            let
                newModel =
                    { model | mileageAgreement = String.toInt amount |> Result.withDefault 0 }
            in
                ( newModel, Cmd.none )

        AprChanged amount ->
            let
                newModel =
                    { model | apr = String.toFloat (amount) |> Result.withDefault 0.0 }
            in
                ( newModel, Cmd.none )



-- Subscriptions


{-| There are no subscriptions but we need to define the function
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


{-| A view represneting a slider component
-}
viewSlider : Model -> (String -> Msg) -> String -> String -> String -> { min : String, max : String, step : String, value : String } -> Html Msg
viewSlider model message labelText valuePostfix className sliderProperties =
    div [ class "pcp-calculator-ranges-heading" ]
        [ div [ class "pcp-calculator-range-heading" ] [ div [ class "pcp-calculator-range__label" ] [ text labelText ], div [ class "pcp-calculator-range__value" ] [ text <| sliderProperties.value ++ valuePostfix ] ]
        , div [ class "form-input" ]
            [ div [ class "input-slider" ]
                [ input [ class "slider-value", type_ "range", name "length-of-finance", Html.Attributes.min sliderProperties.min, Html.Attributes.max sliderProperties.max, Html.Attributes.step sliderProperties.step, value sliderProperties.value, onInput message ] [] ]
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
            , div [ class "pcp-finance-amount-financed__amount" ] [ text (formatCurrency "£" (netLoan model)) ]
            ]
        , div [ class "pcp-calculator-form__ranges" ]
            [ viewSlider model FinanceLengthChanged "Finance length" " months" "length-of-finance" { min = "12", max = "48", step = "6", value = toString model.financeLength }
            , viewSlider model MileageAgreementChanged "Mileage agreement" " miles" "mileage-agreement" { min = "5000", max = "30000", step = "5000", value = toString model.mileageAgreement }
            , viewSlider model AprChanged "APR %" " %" "apr" { min = "0.0", max = "20.0", step = "0.05", value = toString model.apr }
            ]
        , div [ class "pcp-calculator-form__estimated-monthly-payment" ]
            [ div [ class "pcp-calculator__payment-label" ] [ text "Estimated monthly payment" ]
            , div [ class "pcp-calculator__payment-amount" ] [ text (formatCurrency "£" (monthlyPayment model)) ]
            ]
        ]



-- Functions


netLoan : Model -> Float
netLoan inputs =
    toFloat (inputs.carPrice - inputs.deposit - inputs.dealerContribution - inputs.finalPayment)


monthlyRate : Model -> Float
monthlyRate inputs =
    (inputs.apr / 12) / 100


monthlyPayment : Model -> Float
monthlyPayment inputs =
    let
        monthlyAprRate =
            monthlyRate inputs

        financeAmount =
            netLoan inputs
    in
        if inputs.apr == 0 then
            financeAmount / (toFloat inputs.financeLength)
        else
            (financeAmount * monthlyAprRate)
                / (1 - ((1 + monthlyAprRate) ^ (toFloat -inputs.financeLength)))
