module Components.Details exposing
    ( Details
    , Model
    , Msg
    , init
    , new
    , update
    , view
    )

import Airport exposing (Airport)
import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Modal as Modal
import Effect exposing (Effect)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Http


type Details msg
    = Config
        { model : Model
        , toMsg : Msg -> msg
        , onClose : Maybe Airport -> msg
        }


new :
    { model : Model
    , toMsg : Msg -> msg
    , onClose : Maybe Airport -> msg
    }
    -> Details msg
new props =
    Config { model = props.model, toMsg = props.toMsg, onClose = props.onClose }



-- INIT


type Model
    = CompModel
        { modalVisibility : Modal.Visibility
        , airport : Maybe Airport
        }


init : { airport : Maybe Airport } -> Model
init { airport } =
    case airport of
        Just { code, name, country } ->
            CompModel
                { modalVisibility = Modal.shown
                , airport =
                    Just
                        { code = code
                        , name = name
                        , country = country
                        }
                }

        Nothing ->
            CompModel
                { modalVisibility = Modal.hidden
                , airport = Nothing
                }



-- UPDATE


type Msg
    = UserUpdatedInput Field String
    | AnimateModal Modal.Visibility


type Field
    = Name
    | Country


update :
    { msg : Msg
    , model : Model
    , toModel : Model -> model
    }
    -> ( model, Effect msg )
update props =
    let
        (CompModel model) =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case ( props.msg, model.airport ) of
            ( UserUpdatedInput Name value, Just airport ) ->
                ( CompModel { model | airport = Just { airport | name = value } }
                , Effect.none
                )

            ( UserUpdatedInput Country value, Just airport ) ->
                ( CompModel { model | airport = Just { airport | country = value } }
                , Effect.none
                )

            ( AnimateModal visibility, _ ) ->
                ( CompModel { model | modalVisibility = visibility }
                , Effect.none
                )

            _ ->
                ( CompModel model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions (CompModel model) =
    Sub.batch
        [ Modal.subscriptions model.modalVisibility AnimateModal ]



-- VIEW


view : Details msg -> Html msg
view (Config config) =
    let
        (CompModel model) =
            config.model
    in
    case model.airport of
        Just airport ->
            Modal.config (config.onClose Nothing)
                |> Modal.small
                |> Modal.h5 [] [ text "Edit airport" ]
                |> Modal.large
                |> Modal.hideOnBackdropClick False
                |> Modal.body [] [ Form.form [] [ viewForm config.toMsg airport ] ]
                |> Modal.footer [] (viewButtons config.onClose airport)
                |> Modal.view model.modalVisibility

        -- Modal.shown
        Nothing ->
            div [] []


viewButtons onClose airport =
    [ Button.button
        [ Button.outlinePrimary
        , Button.attrs [ onClick (onClose Nothing) ]
        ]
        [ text "Cancel" ]
    , Button.button
        [ Button.primary
        , Button.attrs [ onClick (onClose (Just airport)) ]
        ]
        [ text "Save" ]
    ]


viewForm toMsg airport =
    let
        onUpdateName s =
            toMsg (UserUpdatedInput Name s)

        onUpdateCountry s =
            toMsg (UserUpdatedInput Country s)
    in
    Form.form []
        [ Form.row []
            [ Form.col []
                [ Form.label [] [ text "Code" ]
                , Input.text
                    [ Input.value airport.code -- "08FE037A-DC44-4496-8543-90CA22EB414D"
                    , Input.readonly True
                    ]
                ]
            ]
        , Form.row []
            [ Form.col []
                [ Form.label [] [ text "Name" ]
                , Input.text
                    [ Input.placeholder "short_name"
                    , Input.value airport.name
                    , Input.onInput onUpdateName
                    ]
                ]
            ]
        , Form.row []
            [ Form.col []
                [ Form.label [] [ text "Country" ]
                , Textarea.textarea
                    [ Textarea.rows 4
                    , Textarea.value airport.country
                    , Textarea.onInput onUpdateCountry
                    ]
                ]
            ]
        ]
