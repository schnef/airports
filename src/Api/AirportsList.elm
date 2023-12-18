module Api.AirportsList exposing (get, getAll, put)

import Airport exposing (Airport)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


put : { airport : Airport, onResponse : Result Http.Error () -> msg } -> Cmd msg
put options =
    let
        { code, name, country } =
            options.airport

        url =
            Debug.log "url" ("http://localhost:8080/airport/" ++ code)

        body =
            Http.jsonBody <|
                Encode.object
                    [ ( "code", Encode.string code )
                    , ( "name", Encode.string name )
                    , ( "country", Encode.string country )
                    ]
    in
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectWhatever options.onResponse
        , timeout = Nothing
        , tracker = Nothing
        }


get : { code : String, onResponse : Result Http.Error Airport -> msg } -> Cmd msg
get options =
    Http.get
        { url = "http://localhost:8080/airport/" ++ options.code
        , expect = Http.expectJson options.onResponse airportDecoder
        }


getAll : { onResponse : Result Http.Error (List Airport) -> msg } -> Cmd msg
getAll options =
    Http.get
        { url = "http://localhost:8080/airport"
        , expect = Http.expectJson options.onResponse airportsDecoder
        }


airportsDecoder : Decode.Decoder (List Airport)
airportsDecoder =
    Decode.field "airports" (Decode.list airportDecoder)


airportDecoder : Decode.Decoder Airport
airportDecoder =
    Decode.field "airport"
        (Decode.map3 Airport
            codeFieldDecoder
            nameFieldDecoder
            countryFieldDecoder
        )


codeFieldDecoder : Decode.Decoder String
codeFieldDecoder =
    Decode.field "code" Decode.string


nameFieldDecoder : Decode.Decoder String
nameFieldDecoder =
    Decode.field "name" Decode.string


countryFieldDecoder : Decode.Decoder String
countryFieldDecoder =
    Decode.field "country" Decode.string
