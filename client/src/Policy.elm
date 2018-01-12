module Policy exposing (Id, Policy, decoder, id)

import Json.Decode as D
import Tagged exposing (Tagged)


type alias Policy =
    { id : Id
    , title : String
    , text : String
    }


type alias Id =
    Tagged IdTag Int


type IdTag
    = IdTag


id : Int -> Id
id =
    Tagged.tag


decoder : D.Decoder Policy
decoder =
    D.map3 Policy
        (D.field "id" D.int |> D.map Tagged.tag)
        (D.field "title" D.string)
        (D.field "text" D.string)
