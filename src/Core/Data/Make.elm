module Core.Data.Make exposing (Make, listDecoder, decoder)

{-| Make menu component


# Exported

@docs Make, listDecoder, decoder

-}

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional)


{-| A representation of the Make
-}
type alias Make =
    { name : String
    , slug : String
    , leaseDealsAvailable : Bool
    , stockAvailable : Bool
    , factoryOrderAvailable : Bool
    }


{-| Decode the list of Makes
-}
listDecoder : Decoder (List Make)
listDecoder =
    list decoder


{-| Make decoder
-}
decoder : Decoder Make
decoder =
    decode Make
        |> required "name" string
        |> required "slug" string
        |> optional "lease_deals_available" bool True
        |> optional "stock_available" bool True
        |> optional "factory_order_available" bool True
