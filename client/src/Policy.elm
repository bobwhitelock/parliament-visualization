module Policy exposing (..)

import Json.Decode as D


type alias Policy =
    { id : Id
    , title : String
    , text : String
    }


type Id
    = Id Int


decoder : D.Decoder Policy
decoder =
    D.map3 Policy
        (D.field "id" D.int |> D.map Id)
        (D.field "title" D.string)
        (D.field "text" D.string)
