module VoteEvent
    exposing
        ( PersonId
        , VoteEvent
        , decoder
        , encode
        , partyColour
        , partyComplementaryColour
        , personId
        )

import Color exposing (Color)
import Color.Convert
import Color.Manipulate
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra
import Tagged exposing (Tagged)
import VoteOption exposing (VoteOption)


type alias VoteEvent =
    { personId : PersonId
    , name : String
    , party : String
    , option : VoteOption
    }


type alias PersonId =
    Tagged PersonIdTag Int


type PersonIdTag
    = PersonIdTag


personId : Int -> PersonId
personId =
    Tagged.tag


decoder : D.Decoder VoteEvent
decoder =
    D.map4 VoteEvent
        (D.field "person_id" D.int |> D.map Tagged.tag)
        (D.field "name" D.string)
        (D.field "party" D.string)
        (D.field "option" VoteOption.decoder)


encode : Maybe PersonId -> VoteEvent -> E.Value
encode selectedPersonId event =
    let
        borderColourValue =
            case personBorderColour selectedPersonId event of
                Just colour ->
                    E.string colour

                Nothing ->
                    E.null
    in
    E.object
        [ ( "personId", Tagged.untag event.personId |> E.int )
        , ( "name", E.string event.name )
        , ( "colour", personColour selectedPersonId event |> E.string )
        , ( "borderColour", borderColourValue )
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


personColour : Maybe PersonId -> VoteEvent -> String
personColour selectedPersonId =
    rawPersonColour selectedPersonId >> Color.Convert.colorToHex


rawPersonColour : Maybe PersonId -> VoteEvent -> Color
rawPersonColour maybeSelectedPersonId event =
    let
        partyColour =
            rawPartyColour event

        alterColourIfSelected =
            \selectedPersonId ->
                if event.personId == selectedPersonId then
                    Color.Manipulate.lighten 0.1 partyColour
                else
                    partyColour
    in
    Maybe.map alterColourIfSelected maybeSelectedPersonId
        |> Maybe.withDefault partyColour


personBorderColour : Maybe PersonId -> VoteEvent -> Maybe String
personBorderColour selectedPersonId voteEvent =
    rawPersonBorderColour selectedPersonId voteEvent
        |> Maybe.map Color.Convert.colorToHex


rawPersonBorderColour : Maybe PersonId -> VoteEvent -> Maybe Color
rawPersonBorderColour maybeSelectedPersonId event =
    let
        setBorderIfSelected =
            \selectedPersonId ->
                if event.personId == selectedPersonId then
                    rawPartyComplementaryColour event |> Just
                else
                    Nothing
    in
    Maybe.map setBorderIfSelected maybeSelectedPersonId
        |> Maybe.Extra.join


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


partyComplementaryColour : VoteEvent -> String
partyComplementaryColour =
    rawPartyComplementaryColour >> Color.Convert.colorToHex


rawPartyComplementaryColour : VoteEvent -> Color
rawPartyComplementaryColour event =
    if isSpeaker event then
        -- Showing speaker's party colour as black, so show text as
        -- white so can see it.
        Color.white
    else
        Color.black
