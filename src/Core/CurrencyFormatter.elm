module Core.CurrencyFormatter exposing (formatCurrency)

{-| CurrencyFormatter Module

A helper module to format currency strings


# Exported

@docs formatCurrency

-}

import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)


integerUsLocale : Int -> Locale
integerUsLocale numDecimals =
    { usLocale
        | decimals = numDecimals
    }


{-| Formats the currency string
-}
formatCurrency : String -> Float -> String
formatCurrency currencySymbol number =
    currencySymbol ++ format (integerUsLocale 0) number
