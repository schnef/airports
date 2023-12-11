module Api.AirportsList exposing (getAll)

import Airport exposing (Airport)
import Http
import Json.Decode


getAll : { onResponse : Result Http.Error (List Airport) -> msg } -> Cmd msg
getAll options =
    Http.get
        { url = "http://localhost:8080/airport"
        , expect = Http.expectJson options.onResponse airportsDecoder
        }


airportsDecoder : Json.Decode.Decoder (List Airport)
airportsDecoder =
    Json.Decode.field "airports" (Json.Decode.list airportDecoder)


airportDecoder : Json.Decode.Decoder Airport
airportDecoder =
    Json.Decode.field "airport"
        (Json.Decode.map3 Airport
            codeFieldDecoder
            nameFieldDecoder
            countryFieldDecoder
        )


codeFieldDecoder : Json.Decode.Decoder String
codeFieldDecoder =
    Json.Decode.field "code" Json.Decode.string


nameFieldDecoder : Json.Decode.Decoder String
nameFieldDecoder =
    Json.Decode.field "name" Json.Decode.string


countryFieldDecoder : Json.Decode.Decoder String
countryFieldDecoder =
    Json.Decode.field "country" Json.Decode.string
