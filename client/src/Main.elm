port module Main exposing (..)

import Date.Extra
import EveryDict as Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Maybe.Extra
import RemoteData exposing (RemoteData(..), WebData)
import Svg
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes exposing (..)
import Vote exposing (Vote)
import VoteEvent exposing (VoteEvent)
import Votes exposing (Votes)


-- PORTS --


port chartData : E.Value -> Cmd msg


port personNodeHovered : (Int -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { votes : WebData Votes
    , chartVoteId : Maybe Vote.Id
    , voteInput : String
    , hoveredPersonId : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { votes = NotAsked
      , chartVoteId = Nothing
      , voteInput = ""
      , hoveredPersonId = Nothing
      }
    , getInitialVotes
    )


getInitialVotes : Cmd Msg
getInitialVotes =
    Http.get "/votes" Votes.decoder
        |> RemoteData.sendRequest
        |> Cmd.map InitialVotesResponse



---- UPDATE ----


type Msg
    = InitialVotesResponse (WebData Votes)
    | VoteEventsResponse Vote.Id (WebData (List VoteEvent))
    | VoteChanged String
    | ShowVote Vote.Id
    | PersonNodeHovered Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialVotesResponse votes ->
            let
                newModel =
                    { model | votes = votes }
            in
            handleVoteStateChange newModel

        VoteEventsResponse voteId response ->
            case model.votes of
                Success { selected, data } ->
                    let
                        newVotes =
                            Votes selected
                                (Dict.update
                                    voteId
                                    (Maybe.map (\vote -> { vote | voteEvents = response }))
                                    data
                                )
                                |> Success

                        newModel =
                            { model | votes = newVotes }
                    in
                    handleVoteStateChange newModel

                _ ->
                    model ! []

        VoteChanged input ->
            { model | voteInput = input } ! []

        ShowVote newVoteId ->
            case model.votes of
                Success { data } ->
                    let
                        newVotes =
                            Votes newVoteId data |> Success

                        newModel =
                            { model | votes = newVotes }
                    in
                    handleVoteStateChange newModel

                _ ->
                    model ! []

        PersonNodeHovered personId ->
            { model | hoveredPersonId = Just personId } ! []


{-|

    Handle in a standard way updating model and making HTTP requests/sending
    graph data through port, when either selected vote or Votes data changes.

-}
handleVoteStateChange : Model -> ( Model, Cmd Msg )
handleVoteStateChange model =
    case model.votes of
        Success votes ->
            case Votes.selected votes of
                Just vote ->
                    let
                        { previous, next } =
                            Votes.neighbouringVotes votes
                                |> Maybe.withDefault
                                    { previous = Nothing, next = Nothing }

                        maybeCmdToGet =
                            \maybeVote ->
                                Maybe.map cmdToGetEvents maybeVote
                                    |> Maybe.Extra.join

                        neighbouringVotesToGet =
                            Maybe.Extra.values
                                [ maybeCmdToGet previous, maybeCmdToGet next ]
                    in
                    case vote.voteEvents of
                        Success voteEvents ->
                            { model | chartVoteId = Just vote.id }
                                ! (sendChartData vote :: neighbouringVotesToGet)

                        NotAsked ->
                            let
                                newVotesData =
                                    Dict.update
                                        vote.id
                                        (Maybe.map
                                            (\vote -> { vote | voteEvents = Loading })
                                        )
                                        votes.data

                                newVotes =
                                    Votes votes.selected newVotesData |> Success

                                newModel =
                                    { model | votes = newVotes }
                            in
                            newModel ! (getEventsForVote vote.id :: neighbouringVotesToGet)

                        Failure _ ->
                            -- XXX Handle this somewhere?
                            model ! neighbouringVotesToGet

                        Loading ->
                            model ! neighbouringVotesToGet

                Nothing ->
                    model ! []

        _ ->
            model ! []


cmdToGetEvents : Vote -> Maybe (Cmd Msg)
cmdToGetEvents vote =
    if RemoteData.isNotAsked vote.voteEvents then
        getEventsForVote vote.id |> Just
    else
        Nothing


getEventsForVote : Vote.Id -> Cmd Msg
getEventsForVote voteId =
    let
        (Vote.Id id) =
            voteId

        path =
            "/vote-events/" ++ toString id
    in
    Http.get path (D.list VoteEvent.decoder)
        |> RemoteData.sendRequest
        |> Cmd.map (VoteEventsResponse voteId)


sendChartData : Vote -> Cmd msg
sendChartData vote =
    Vote.chartDataValue vote |> chartData



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.votes of
        Success votes ->
            viewVotes model.hoveredPersonId votes

        Failure error ->
            div [] [ "Error loading data: " ++ toString error |> text ]

        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]


viewVotes : Maybe Int -> Votes -> Html Msg
viewVotes hoveredPersonId votes =
    case ( Votes.selected votes, Votes.neighbouringVotes votes ) of
        ( Just current, Just { previous, next } ) ->
            let
                previousVoteButton =
                    voteNavigationButton previous "<"

                nextVoteButton =
                    voteNavigationButton next ">"

                voteNavigationButton =
                    \maybeVote ->
                        \icon ->
                            case maybeVote of
                                Just { id } ->
                                    button [ onClick (ShowVote id) ] [ text icon ]

                                Nothing ->
                                    span [] []

                hoveredPersonEvent =
                    case ( current.voteEvents, hoveredPersonId ) of
                        ( Success events, Just personId ) ->
                            List.Extra.find
                                (.personId >> (==) personId)
                                events

                        _ ->
                            Nothing

                hoveredPersonText =
                    case hoveredPersonEvent of
                        Just event ->
                            event.name
                                ++ " | "
                                ++ event.party
                                ++ " | "
                                ++ toString event.option

                        Nothing ->
                            "Nobody"

                chartClasses =
                    if RemoteData.isLoading current.voteEvents then
                        [ o_70 ]
                    else
                        []

                chart =
                    div [ classes chartClasses ]
                        [ Svg.svg
                            [ width 1000
                            , height 800
                            , id "d3-simulation"
                            ]
                            []
                        ]
            in
            div []
                [ tachyons.css
                , div []
                    [ "Current vote: "
                        ++ current.policyTitle
                        ++ " | "
                        ++ current.text
                        ++ " | "
                        ++ Date.Extra.toFormattedString "ddd MMMM, y" current.date
                        |> text
                    ]
                , div [] [ "Hovered over: " ++ hoveredPersonText |> text ]
                , div []
                    [ previousVoteButton, nextVoteButton ]
                , chart
                ]

        _ ->
            div [] [ text "No votes available." ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    personNodeHovered PersonNodeHovered



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
