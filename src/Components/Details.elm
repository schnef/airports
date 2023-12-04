module Components.Details exposing
    ( Details
    , Model
    , Msg
    , init
    , new
    , update
    , view
    )

import Bootstrap.Button as Button
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Radio as Radio
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Modal as Modal
import Effect exposing (Effect)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)



-- INIT


type Details msg
    = Config
        { model : Model
        , toMsg : Msg -> msg
        , onClose : msg
        }


new :
    { model : Model
    , toMsg : Msg -> msg
    , onClose : msg
    , airport : { code : String, name : String, country : String }
    }
    -> Details msg
new props =
    Config { model = props.model, toMsg = props.toMsg, onClose = props.onClose }



-- INIT


type Model
    = CompModel
        { code : Maybe String
        , name : String
        , country : String
        }


init : Model
init =
    CompModel
        { code = Nothing
        , name = ""
        , country = ""
        }



-- UPDATE


type Msg
    = UserUpdatedInput Field String


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
        case props.msg of
            UserUpdatedInput Name value ->
                ( CompModel { model | name = value }
                , Effect.none
                )

            UserUpdatedInput Country value ->
                ( CompModel { model | country = value }
                , Effect.none
                )


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- VIEW


view : Details msg -> Html msg
view (Config config) =
    Modal.config config.onClose
        |> Modal.small
        |> Modal.h5 [] [ text "Edit airport" ]
        |> Modal.large
        |> Modal.hideOnBackdropClick False
        |> Modal.body [] [ Form.form [] [ viewForm config ] ]
        |> Modal.footer [] (viewButtons config)
        |> Modal.view Modal.shown


viewButtons config =
    [ Button.button
        [ Button.outlinePrimary
        , Button.attrs [ onClick config.onClose ]
        ]
        [ text "Cancel" ]
    , Button.button
        [ Button.primary
        , Button.attrs [ onClick config.onClose ]
        ]
        [ text "Save" ]
    ]


viewForm config =
    let
        (CompModel model) =
            config.model

        onUpdateName s =
            config.toMsg (UserUpdatedInput Name s)

        onUpdateCountry s =
            config.toMsg (UserUpdatedInput Country s)

        code =
            case model.code of
                Just s ->
                    s

                Nothing ->
                    "<nothing>"
    in
    Form.form []
        [ Form.row []
            [ Form.col []
                [ Form.label [] [ text "Code" ]
                , Input.text
                    [ Input.value code -- "08FE037A-DC44-4496-8543-90CA22EB414D"
                    , Input.readonly True
                    ]
                ]
            ]
        , Form.row []
            [ Form.col []
                [ Form.label [] [ text "Name" ]
                , Input.text
                    [ Input.placeholder "short_name"
                    , Input.value model.name
                    , Input.onInput onUpdateName
                    ]
                ]
            ]
        , Form.row []
            [ Form.col []
                [ Form.label [] [ text "Country" ]
                , Textarea.textarea
                    [ Textarea.rows 4
                    , Textarea.value model.country
                    , Textarea.onInput onUpdateCountry
                    ]
                ]
            ]
        ]
