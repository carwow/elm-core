module PcpCalculator exposing (main)

import Html exposing (Html, Attribute, div, input, text, fieldset, label)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)
import String


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


main =
    Html.beginnerProgram { model = model, view = view, update = update }



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


model : Model
model =
    { carPrice = 0
    , deposit = 0
    , dealerContribution = 0
    , finalPayment = 0
    , financeLength = 36
    , mileageAgreement = 10000
    , apr = 7.0
    }



-- Update


type Msg
    = CarPriceChanged String
    | DepositChanged String
    | DealerContributionChanged String
    | FinalPaymentChanged String
    | FinanceLengthChanged String
    | MileageAgreementChanged String
    | AprChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CarPriceChanged amount ->
            { model | carPrice = (Result.withDefault 0 (String.toInt (amount))) }

        DepositChanged amount ->
            { model | deposit = (Result.withDefault 0 (String.toInt (amount))) }

        DealerContributionChanged amount ->
            { model | dealerContribution = (Result.withDefault 0 (String.toInt (amount))) }

        FinalPaymentChanged amount ->
            { model | finalPayment = (Result.withDefault 0 (String.toInt (amount))) }

        FinanceLengthChanged amount ->
            { model | financeLength = (Result.withDefault 0 (String.toInt (amount))) }

        MileageAgreementChanged amount ->
            { model | mileageAgreement = (Result.withDefault 0 (String.toInt (amount))) }

        AprChanged amount ->
            { model | apr = (Result.withDefault 0.0 (String.toFloat (amount))) }



-- View


view : Model -> Html Msg
view model =
    fieldset []
        [ div []
            [ label []
                [ text "Car Price"
                , input [ type_ "text", name "car-price", placeholder ("i.e. £20,000"), onInput CarPriceChanged ] []
                ]
            ]
        , div []
            [ label []
                [ text "Your Deposit"
                , input [ type_ "text", name "your-deposit", placeholder ("i.e. £2,500"), onInput DepositChanged ] []
                ]
            ]
        , div []
            [ label []
                [ text "Dealer Contribution"
                , input [ type_ "text", name "dealer-contribution", placeholder ("i.e. £1,000"), onInput DealerContributionChanged ] []
                ]
            ]
        , div []
            [ label []
                [ text "Final Payment"
                , input [ type_ "text", name "final-payment", placeholder ("i.e. £1,000"), onInput FinalPaymentChanged ] []
                ]
            ]
        , div []
            [ label []
                [ text "Length of finance"
                , input [ type_ "range", name "length-of-finance", H.min "12", H.max "48", H.step "6", value <| toString model.financeLength, onInput FinanceLengthChanged ] []
                ]
            , text <| toString model.financeLength
            ]
        , div []
            [ label []
                [ text "Mileage agreement"
                , input [ type_ "range", name "mileage-agreement", H.min "5000", H.max "30000", H.step "5000", value <| toString model.mileageAgreement, onInput MileageAgreementChanged ] []
                ]
            , text <| toString model.mileageAgreement
            ]
        , div []
            [ label []
                [ text "APR %"
                , input [ type_ "range", name "apr", H.min "0.01", H.max "20.00", H.step "0.01", value <| toString model.apr, onInput AprChanged ] []
                ]
            , text <| toString model.apr
            ]
        , div [] [ text (formatPaymentCurrency (monthlyPayment model)) ]
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
