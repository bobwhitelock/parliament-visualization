module Vote exposing (..)

import Date exposing (Date)
import Date.Extra
import Json.Decode as D
import Json.Encode as E
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import VoteEvent exposing (VoteEvent)


type alias Vote =
    { id : Id
    , policyTitle : String
    , text : String
    , actionsYes : String
    , actionsNo : String
    , date : Date
    , voteEvents : WebData (List VoteEvent)
    }


type Id
    = Id Int


chartDataValue : Maybe Int -> Vote -> E.Value
chartDataValue selectedPersonId vote =
    case vote.voteEvents of
        Success events ->
            E.list (List.map (VoteEvent.encode selectedPersonId) events)

        _ ->
            -- XXX Handle this better.
            E.null


eventForPersonId : Vote -> Maybe Int -> Maybe VoteEvent
eventForPersonId vote personId =
    case ( vote.voteEvents, personId ) of
        ( Success events, Just personId_ ) ->
            List.Extra.find
                (.personId >> (==) personId_)
                events

        _ ->
            Nothing


withoutEventsDecoder : D.Decoder (Maybe Vote)
withoutEventsDecoder =
    let
        initialVoteState =
            \id ->
                \policyTitle ->
                    \text ->
                        \actionsYes ->
                            \actionsNo ->
                                \date ->
                                    case Date.Extra.fromIsoString date of
                                        Just date_ ->
                                            Vote
                                                id
                                                policyTitle
                                                text
                                                actionsYes
                                                actionsNo
                                                date_
                                                NotAsked
                                                |> Just

                                        Nothing ->
                                            Nothing
    in
    D.map6 initialVoteState
        (D.field "id" D.int |> D.map Id)
        (D.field "policy_title" D.string)
        (D.field "text" D.string)
        (D.field "actions_yes" D.string)
        (D.field "actions_no" D.string)
        (D.field "date" D.string)


withEventsDecoder : D.Decoder (Maybe Vote)
withEventsDecoder =
    -- XXX de-duplicate this and above.
    let
        createVote =
            \id ->
                \policyTitle ->
                    \text ->
                        \actionsYes ->
                            \actionsNo ->
                                \date ->
                                    \voteEvents ->
                                        case Date.Extra.fromIsoString date of
                                            Just date_ ->
                                                Vote
                                                    id
                                                    policyTitle
                                                    text
                                                    actionsYes
                                                    actionsNo
                                                    date_
                                                    voteEvents
                                                    |> Just

                                            Nothing ->
                                                Nothing
    in
    D.map7 createVote
        (D.field "id" D.int |> D.map Id)
        (D.field "policy_title" D.string)
        (D.field "text" D.string)
        (D.field "actions_yes" D.string)
        (D.field "actions_no" D.string)
        (D.field "date" D.string)
        (D.field "voteEvents" (D.list VoteEvent.decoder |> D.map Success))
