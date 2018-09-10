module Main exposing (main)

import Browser
import Html exposing (Html, Attribute, button, div, ul, li, strong, form, input, span, text)
import Html.Attributes exposing (class, placeholder, value, type_, autofocus)
import Html.Events exposing (onSubmit, onInput)
import Port exposing (..)


type alias Model =
    { messages : List Message, currentUser : User, currentMessage : String }


init : String -> ( Model, Cmd Msg )
init userName =
    ( { messages = []
      , currentUser = userName
      , currentMessage = ""
      }
    , Cmd.none
    )


type Msg
    = UpdateMessage String
    | SubmitMessage
    | ReceivedDataFromJS Message


addMessage : List Message -> Message -> List Message
addMessage list message =
    message :: list


submitMessage : Message -> Model -> Model
submitMessage message ({ messages } as model) =
    { model | messages = addMessage messages message }


clearCurrentMessage : Model -> Model
clearCurrentMessage model =
    { model | currentMessage = "" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateMessage text ->
            ( { model | currentMessage = text }, Cmd.none )

        SubmitMessage ->
            let
                message =
                    Message model.currentUser model.currentMessage
            in
                ( model
                    |> submitMessage message
                    |> clearCurrentMessage
                , dispatchMessage (encodeMessage message)
                )

        ReceivedDataFromJS message ->
            ( { model | messages = addMessage model.messages message }, Cmd.none )


viewFeed : Model -> Html Msg
viewFeed model =
    ul [ class "chat__feed" ] (List.map (viewMessage model) model.messages)


viewMessage : Model -> Message -> Html Msg
viewMessage model message =
    let
        classes =
            if model.currentUser == message.user then
                { line = "chat__feed__line chat__feed__line--current"
                , text = "chat__feed__line__text chat__feed__line__text--current"
                }
            else
                { line = "chat__feed__line", text = "chat__feed__line__text" }
    in
        li
            [ class classes.line ]
            [ strong
                [ class "chat__feed__line__user" ]
                [ text message.user ]
            , span [ class classes.text ] [ text message.text ]
            ]


viewForm : Model -> Html Msg
viewForm model =
    form [ class "chat__form", onSubmit SubmitMessage ]
        [ input
            [ class "chat__form__input"
            , placeholder (model.currentUser ++ " type a message and press enter")
            , value model.currentMessage
            , onInput UpdateMessage
            , autofocus True
            ]
            []
        , button [ class "chat__form__submit", type_ "submit" ] [ text "OK" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "chat" ]
        [ viewFeed model
        , viewForm model
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveMessage ReceivedDataFromJS


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
