module Votes
    exposing
        ( NeighbouringVotes
        , Votes
        , decoder
        , neighbouringVotes
        , selected
        )

import Date.Extra
import EveryDict as Dict exposing (EveryDict)
import Json.Decode as D
import Maybe.Extra
import Policy exposing (Policy)
import SelectList exposing (SelectList)
import Vote exposing (Vote)


type alias Votes =
    { selected : Vote.Id
    , data : EveryDict Vote.Id Vote
    , policies : EveryDict Policy.Id Policy
    }


selected : Votes -> Maybe Vote
selected { selected, data } =
    Dict.get selected data


type alias NeighbouringVotes =
    { previous : Maybe Vote
    , next : Maybe Vote
    }


neighbouringVotes : Votes -> Maybe NeighbouringVotes
neighbouringVotes votes =
    case timeOrdered votes of
        Just orderedVotes ->
            let
                previousVote =
                    SelectList.before orderedVotes
                        |> List.reverse
                        |> List.head

                nextVote =
                    SelectList.after orderedVotes
                        |> List.head
            in
            Just
                { previous = previousVote
                , next = nextVote
                }

        Nothing ->
            Nothing


timeOrdered : Votes -> Maybe (SelectList Vote)
timeOrdered { selected, data } =
    -- XXX Remove use of SelectList here; not really necessary?
    let
        compare =
            \vote1 -> \vote2 -> Date.Extra.compare vote1.date vote2.date

        orderedVotes =
            Dict.toList data
                |> List.map Tuple.second
                |> List.sortWith compare
    in
    -- XXX Use SelectList.fromList once exists.
    case ( List.head orderedVotes, List.tail orderedVotes ) of
        ( Just head, Just tail ) ->
            SelectList.fromLists [] head tail
                |> SelectList.select (\vote -> vote.id == selected)
                |> Just

        _ ->
            Nothing


decoder : D.Decoder Votes
decoder =
    let
        createInitialVotes =
            \votes ->
                \latestVote ->
                    \policies ->
                        let
                            -- Every Vote should have a date, but need to filter
                            -- out any which somehow didn't to ensure this.
                            votesWithDates =
                                Maybe.Extra.values votes
                        in
                        Maybe.map
                            (\latest ->
                                Votes latest.id
                                    (createVotesDict votesWithDates
                                        |> Dict.insert latest.id latest
                                    )
                                    (createPoliciesDict policies)
                                    |> Just
                            )
                            latestVote
                            |> Maybe.Extra.join

        createVotesDict =
            \votes ->
                List.map
                    (\vote -> ( vote.id, vote ))
                    votes
                    |> Dict.fromList

        createPoliciesDict =
            \policies ->
                List.map
                    (\policy -> ( policy.id, policy ))
                    policies
                    |> Dict.fromList

        maybeVotesToDecoder =
            \maybeVotes ->
                case maybeVotes of
                    Just votes ->
                        D.succeed votes

                    Nothing ->
                        D.fail "Failed to decode initial data"
    in
    D.map3 createInitialVotes
        (D.field "votes" (D.list Vote.withoutEventsDecoder))
        (D.field "latestVote" Vote.withEventsDecoder)
        (D.field "policies" (D.list Policy.decoder))
        |> D.andThen maybeVotesToDecoder
