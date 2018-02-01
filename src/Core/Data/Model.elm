module Core.Data.Model exposing (Model, listDecoder, decoder)

{-| Model menu component


# Exported

@docs Model, listDecoder, decoder

-}

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional)


{-| A representation of the Model
-}
type alias Model =
    { name : String
    , slug : String
    , leaseDealsAvailable : Bool
    , stockAvailable : Bool
    , factoryOrderAvailable : Bool
    }


{-| Decode the list of Models
-}
listDecoder : Decoder (List Model)
listDecoder =
    list decoder


{-| Model decoder
-}
decoder : Decoder Model
decoder =
    decode Model
        |> required "name" string
        |> required "slug" string
        |> optional "lease_deals_available" bool True
        |> optional "stock_available" bool True
        |> optional "factory_order_available" bool True
