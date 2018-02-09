module PcpCalculator exposing (main)

import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String

main =
  Html.beginnerProgram { model = model, view = view, update = update }


type alias Model =
    {
      carPrice : Int,
      deposit : Int,
      dealerContribution : Int,
      finalPayment : Int,
      financeLength : Int,
      mileageAgreement : Int,
      apr : Float
    }


model : Model
model =
    {
      carPrice = 5000,
      deposit = 1000,
      dealerContribution = 1000,
      finalPayment = 1000,
      financeLength = 36,
      mileageAgreement = 1000,
      apr = 5.0
    }


netLoan : Model -> Int
netLoan inputs =
    inputs.carPrice - inputs.deposit - inputs.dealerContribution - inputs.finalPayment


monthlyRate : Model -> Float
monthlyRate inputs =
  (inputs.apr / 12) / 100


monthlyPayment : Model -> Int
monthlyPayment inputs =
  let
    monthlyAprRate =
      monthlyRate inputs

    financeAmount =
      toFloat(netLoan inputs)
  in
    ceiling((financeAmount * (monthlyAprRate)) /
      (1 - ((1 + monthlyAprRate) ^ toFloat(-inputs.financeLength))) +
        (toFloat(inputs.finalPayment) * (monthlyAprRate)))


type Msg =
    CarPriceChanged String
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
          { model | carPrice = (Result.withDefault 0 ( String.toInt(amount))) }
        DepositChanged amount ->
          { model | deposit = (Result.withDefault 0 ( String.toInt(amount))) }
        DealerContributionChanged amount ->
          { model | dealerContribution = (Result.withDefault 0 ( String.toInt(amount))) }
        FinalPaymentChanged amount ->
          { model | finalPayment = (Result.withDefault 0 ( String.toInt(amount))) }
        FinanceLengthChanged amount ->
          { model | financeLength = (Result.withDefault 0 ( String.toInt(amount))) }
        MileageAgreementChanged amount ->
          { model | mileageAgreement = (Result.withDefault 0 ( String.toInt(amount))) }
        AprChanged amount ->
          { model | apr = (Result.withDefault 0.0 ( String.toFloat(amount))) }


view : Model -> Html Msg
view model =
    div []
      [ input [ placeholder("Car Price"), onInput CarPriceChanged ] []
      , input [ placeholder("Deposit"), onInput DepositChanged ] []
      , input [ placeholder("Dealer Contribution"), onInput DealerContributionChanged ] []
      , input [ placeholder("Final Payment"), onInput FinalPaymentChanged ] []
      , input [ placeholder("Finance Length"), onInput FinanceLengthChanged ] []
      , input [ placeholder("Mileage Agreement"), onInput MileageAgreementChanged ] []
      , input [ placeholder("Apr"), onInput AprChanged ] []
      , div [] [ text (toString(monthlyPayment model)) ]
      ]
