port module Port exposing (..)

import Json.Encode as Encode


type alias User =
    String


type alias Message =
    { user : User, text : String }


encodeMessage : Message -> Encode.Value
encodeMessage message =
    Encode.object
        [ ( "user", Encode.string message.user )
        , ( "text", Encode.string message.text )
        ]


port dispatchMessage : Encode.Value -> Cmd msg


port receiveMessage : (Message -> msg) -> Sub msg
