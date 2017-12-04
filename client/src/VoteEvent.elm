module VoteEvent exposing (VoteEvent, decoder, encode, isSpeaker, partyColour)

import Color exposing (Color)
import Color.Convert
import Json.Decode as D
import Json.Encode as E
import VoteOption exposing (VoteOption)


type alias VoteEvent =
    { personId : Int
    , name : String
    , party : String
    , option : VoteOption
    }


decoder : D.Decoder VoteEvent
decoder =
    D.map4 VoteEvent
        (D.field "person_id" D.int)
        (D.field "name" D.string)
        (D.field "party" D.string)
        (D.field "option" VoteOption.decoder)


encode : VoteEvent -> E.Value
encode event =
    E.object
        [ ( "personId", E.int event.personId )
        , ( "name", E.string event.name )
        , ( "partyColour", partyColour event |> E.string )
        , ( "option", toString event.option |> String.toLower |> E.string )
        ]


isSpeaker : VoteEvent -> Bool
isSpeaker event =
    let
        party =
            String.toLower event.party
    in
    case party of
        "speaker" ->
            True

        "deputy speaker" ->
            True

        _ ->
            False


partyColour : VoteEvent -> String
partyColour =
    rawPartyColour >> Color.Convert.colorToHex


rawPartyColour : VoteEvent -> Color
rawPartyColour event =
    let
        party =
            String.toLower event.party

        labour =
            Color.rgb 220 36 31

        speaker =
            Color.rgb 0 0 0

        independent =
            Color.rgb 128 128 128
    in
    -- All colours obtained from Wikipedia.
    case party of
        "labour" ->
            labour

        "labour/co-operative" ->
            labour

        "conservative" ->
            Color.rgb 0 135 220

        "liberal democrat" ->
            Color.rgb 250 166 26

        "scottish national party" ->
            Color.rgb 254 249 135

        "dup" ->
            Color.rgb 212 106 76

        "sinn féin" ->
            Color.rgb 0 136 0

        "plaid cymru" ->
            Color.rgb 0 129 66

        "green" ->
            Color.rgb 106 176 35

        "social democratic and labour party" ->
            Color.rgb 153 255 102

        "alliance" ->
            Color.rgb 246 203 47

        "respect" ->
            Color.rgb 255 0 0

        "uup" ->
            Color.rgb 153 153 255

        "ukip" ->
            Color.rgb 112 20 122

        "ukup" ->
            Color.rgb 153 0 204

        "speaker" ->
            speaker

        "deputy speaker" ->
            speaker

        "independent" ->
            independent

        "independent labour" ->
            independent

        "independent conservative" ->
            independent

        "independent ulster unionist" ->
            independent

        unknown ->
            -- Should never occur since handling all parties in current data.
            Color.rgb 102 51 153
