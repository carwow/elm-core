module PcpCalculator exposing (..)

import Html exposing (Html, Attribute, div, input, text, fieldset, label, program)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)
import String


-- Model


type alias Model =
    { carPrice : Int
    , deposit : Int
    , dealerContribution : Int
    , finalPayment : Int
    , financeLength : Int
    , mileageAgreement : Int
    , apr : Float
    }


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


type Msg
    = CarPriceChanged String
    | DepositChanged String
    | DealerContributionChanged String
    | FinalPaymentChanged String
    | FinanceLengthChanged String
    | MileageAgreementChanged String
    | AprChanged String


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

        FinanceLengthChanged amount ->
            let
                newModel =
                    { model | financeLength = (Result.withDefault 0 (String.toInt (amount))) }
            in
                ( newModel, Cmd.none )

        MileageAgreementChanged amount ->
            let
                newModel =
                    { model | mileageAgreement = (Result.withDefault 0 (String.toInt (amount))) }
            in
                ( newModel, Cmd.none )

        AprChanged amount ->
            let
                newModel =
                    { model | apr = (Result.withDefault 0.0 (String.toFloat (amount))) }
            in
                ( newModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div [ class "pcp-calculator" ]
        [ div [ class "pcp-calculator-form__input" ]
            [ div [ class "pcp-calculator-form-row" ]
                [ label [ class "form-input-label" ]
                    [ text "Car Price"
                    , input [ type_ "text", name "car-price", placeholder ("i.e. £20,000"), onInput CarPriceChanged ] []
                    ]
                ]
            , div [ class "pcp-calculator-form-row" ]
                [ label [ class "form-input-label" ]
                    [ text "Your Deposit"
                    , input [ type_ "text", name "your-deposit", placeholder ("i.e. £2,500"), onInput DepositChanged ] []
                    ]
                ]
            , div [ class "pcp-calculator-form-row" ]
                [ label [ class "form-input-label" ]
                    [ text "Dealer Contribution"
                    , input [ type_ "text", name "dealer-contribution", placeholder ("i.e. £1,000"), onInput DealerContributionChanged ] []
                    ]
                ]
            , div [ class "pcp-calculator-form-row" ]
                [ label [ class "form-input-label" ]
                    [ text "Final Payment"
                    , input [ type_ "text", name "final-payment", placeholder ("i.e. £1,000"), onInput FinalPaymentChanged ] []
                    ]
                ]
            ]
        , div [ class "pcp-calculator-form__ranges" ]
            [ div [ class "pcp-calculator-form-row" ]
                [ label [ class "pcp-calculator__range-label" ]
                    [ text "Length of finance"
                    , input [ type_ "range", name "length-of-finance", H.min "12", H.max "48", H.step "6", value <| toString model.financeLength, onInput FinanceLengthChanged ] []
                    ]
                , text <| toString model.financeLength
                ]
            , div [ class "pcp-calculator-form-row" ]
                [ label [ class "pcp-calculator__range-label" ]
                    [ text "Mileage agreement"
                    , input [ type_ "range", name "mileage-agreement", H.min "5000", H.max "30000", H.step "5000", value <| toString model.mileageAgreement, onInput MileageAgreementChanged ] []
                    ]
                , text <| toString model.mileageAgreement
                ]
            , div [ class "pcp-calculator-form-row" ]
                [ label [ class "pcp-calculator__range-label" ]
                    [ text "APR %"
                    , input [ type_ "range", name "apr", H.min "0.01", H.max "20.00", H.step "0.01", value <| toString model.apr, onInput AprChanged ] []
                    ]
                , text <| toString model.apr
                ]
            ]
        , div [ class "pcp-calculator-form__estimated-monthly-payment" ]
            [ div [ class "pcp-calculator__payment-label" ] [ text "Estimated monthly payment" ]
            , div [ class "pcp-calculator__payment-amount" ] [ text (formatPaymentCurrency (monthlyPayment model)) ]
            ]
        ]



-- Functions


netLoan : Model -> Int
netLoan inputs =
    inputs.carPrice - inputs.deposit - inputs.dealerContribution - inputs.finalPayment


monthlyRate : Model -> Float
monthlyRate inputs =
    (inputs.apr / 12) / 100


monthlyPayment : Model -> Float
monthlyPayment inputs =
    let
        monthlyAprRate =
            monthlyRate inputs

        financeAmount =
            toFloat (netLoan inputs)
    in
        (financeAmount * (monthlyAprRate))
            / (1 - ((1 + monthlyAprRate) ^ toFloat (-inputs.financeLength)))
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



-- Main


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }
