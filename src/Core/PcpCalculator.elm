module PcpCalculator exposing (main)

import Html exposing (Html, Attribute, div, input, text)



type alias Model =
    {
      inputs : Inputs
    }


type alias Inputs =
    {
      carPrice : Int,
      deposit : Int,
      dealerContribution : Int,
      finalPayment : Int,
      financeLength : Int,
      mileageAgreement : Int,
      apr : Float
    }


netLoan : Inputs -> Int
netLoan inputs =
    inputs.carPrice - inputs.deposit - inputs.dealerContribution - inputs.finalPayment


monthlyRate : Inputs -> Float
monthlyRate inputs =
  (inputs.apr / 12) / 100


monthlyPayment : Inputs -> Int
monthlyPayment inputs =
  let
    monthlyAprRate =
      monthlyRate inputs

    financeAmount =
      toFloat(netLoan inputs)
  in
    floor((financeAmount * (monthlyAprRate)) /
      (1 - ((1 + monthlyAprRate) ^ toFloat(-inputs.financeLength))) +
        (toFloat(inputs.finalPayment) * (monthlyAprRate)))


main =
    text (toString (monthlyPayment (Inputs 5000 1000 1000 1000 36 10000 5.0)))
