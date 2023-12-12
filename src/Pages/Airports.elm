module Pages.Airports exposing (Model, Msg, page)

import Airport exposing (Airport)
import Api
import Api.AirportsList
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text
import Components.Details
import Effect exposing (Effect)
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Http
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Table exposing (defaultCustomizations)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { query : String
    , tableState : Table.State
    , airports : Api.Data (List Airport)
    , selectedAirport : Maybe Airport
    , details : Components.Details.Model
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { query = ""
      , tableState = Table.initialSort "Code"
      , airports = Api.Loading
      , selectedAirport = Nothing
      , details = Components.Details.init { airport = Nothing }
      }
    , Effect.sendCmd (Api.AirportsList.getAll { onResponse = AirportsApiResponded })
    )



-- UPDATE


type Msg
    = SetQuery String
    | SetTableState Table.State
    | Selected Airport
    | UpdateDetails Components.Details.Msg
    | CloseDetails (Maybe Airport)
    | AirportsApiResponded (Result Http.Error (List Airport))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        AirportsApiResponded (Ok httpError) ->
            ( { model | airports = Api.Success httpError }
            , Effect.none
            )

        AirportsApiResponded (Err httpError) ->
            ( { model | airports = Api.Failure httpError }
            , Effect.none
            )

        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Effect.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Effect.none
            )

        Selected airport ->
            let
                details =
                    Components.Details.init { airport = Just airport }
            in
            ( { model | details = details, selectedAirport = Just airport }
            , Effect.none
            )

        UpdateDetails innerMsg ->
            Components.Details.update
                { msg = innerMsg
                , model = model.details
                , toModel = \details -> { model | details = details }
                }

        CloseDetails airport ->
            let
                _ =
                    Debug.log "Updated airport : " airport
            in
            ( { model | selectedAirport = Nothing }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Airports"
    , body =
        case model.airports of
            Api.Loading ->
                [ Spinner.spinner
                    [ Spinner.large
                    , Spinner.color Text.secondary
                    ]
                    [ Spinner.srMessage "Loading..." ]
                ]

            Api.Failure httpError ->
                [ div [] [ text (Api.toUserFriendlyMessage httpError) ] ]

            Api.Success airports ->
                [ viewCard model
                , case model.selectedAirport of
                    Just airport ->
                        Components.Details.new
                            { model = model.details
                            , toMsg = UpdateDetails
                            , onClose = CloseDetails
                            }
                            |> Components.Details.view

                    Nothing ->
                        div [] []
                ]
    }


viewCard model =
    Card.config [ Card.outlineInfo ]
        |> Card.block []
            [ Block.titleH1 [] [ text "Airports" ]
            , Block.custom (input [ placeholder "Search by name", onInput SetQuery ] [])
            , Block.custom (viewTable model)
            ]
        |> Card.view


viewTable : Model -> Html Msg
viewTable ({ tableState, query } as model) =
    let
        airports =
            case model.airports of
                Api.Success xs ->
                    xs

                _ ->
                    []

        lowerQuery =
            String.toLower query

        acceptableAirport =
            List.filter (String.contains lowerQuery << String.toLower << .name) airports
    in
    Table.view config tableState acceptableAirport


config : Table.Config Airport Msg
config =
    Table.customConfig
        { toId = .name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Code" .code
            , Table.stringColumn "Name" .name
            , Table.stringColumn "Country" .country
            , editColumn
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = toTableAttrs
            }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ class "table table-striped table-hover table-sm" ]


editColumn : Table.Column Airport Msg
editColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = viewEdit
        , sorter = Table.unsortable
        }


viewEdit : Airport -> Table.HtmlDetails Msg
viewEdit airport =
    Table.HtmlDetails []
        [ Button.button
            [ Button.small, Button.outlinePrimary, Button.onClick (Selected airport) ]
            [ text "Edit " ]
        ]
