module VoteEvent exposing (VoteEvent, decoder, encode, isSpeaker, partyColour)

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
partyColour event =
    let
        party =
            String.toLower event.party

        labour =
            "#DC241f"

        speaker =
            "black"

        independent =
            "grey"
    in
    -- All colours obtained from Wikipedia.
    case party of
        "labour" ->
            labour

        "labour/co-operative" ->
            labour

        "conservative" ->
            "#0087DC"

        "liberal democrat" ->
            "#FAA61A"

        "scottish national party" ->
            "#FEF987"

        "dup" ->
            "#D46A4C"

        "sinn féin" ->
            "#008800"

        "plaid cymru" ->
            "#008142"

        "green" ->
            "#6AB023"

        "social democratic and labour party" ->
            "#99FF66"

        "alliance" ->
            "#F6CB2F"

        "respect" ->
            "red"

        "uup" ->
            "#9999FF"

        "ukip" ->
            "#70147A"

        "ukup" ->
            "#90C"

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
            "rebeccapurple"
