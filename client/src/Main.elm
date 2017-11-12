port module Main exposing (..)

import Date exposing (Date)
import Date.Extra
import EveryDict as Dict exposing (EveryDict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra
import RemoteData exposing (RemoteData(..), WebData)
import SelectList exposing (SelectList)


-- PORTS --


port graphData : E.Value -> Cmd msg


graphDataValue : Vote -> E.Value
graphDataValue vote =
    case vote.voteEvents of
        Success events ->
            E.list (List.map voteEventValue events)

        _ ->
            -- XXX Handle this better.
            E.null


voteEventValue : VoteEvent -> E.Value
voteEventValue event =
    E.object
        [ ( "personId", E.int event.personId )
        , ( "name", E.string event.name )
        , ( "partyColour", partyColour event |> E.string )
        , ( "option", toString event.option |> String.toLower |> E.string )
        ]


partyColour : VoteEvent -> String
partyColour event =
    let
        party =
            Maybe.map String.toLower event.party

        labour =
            "#DC241f"
    in
    -- All colours obtained from Wikipedia.
    case party of
        Just "labour" ->
            labour

        Just "labour/co-operative" ->
            labour

        Just "conservative" ->
            "#0087DC"

        Just "liberal democrat" ->
            "#FAA61A"

        Just "scottish national party" ->
            "#FEF987"

        Just "dup" ->
            "#D46A4C"

        Just "sinn féin" ->
            "#008800"

        Just "plaid cymru" ->
            "#008142"

        Just "green" ->
            "#6AB023"

        Just "speaker" ->
            "black"

        Just "independent" ->
            "grey"

        Nothing ->
            "black"

        Just unknown ->
            let
                log =
                    Debug.log "Unhandled party: " unknown
            in
            "rebeccapurple"



---- MODEL ----


type alias Model =
    { votes : WebData Votes
    , graphVoteId : Maybe VoteId
    , voteInput : String
    }


type alias Votes =
    { selected : VoteId
    , data : EveryDict VoteId Vote
    }


type alias Vote =
    { id : VoteId
    , policyTitle : String
    , text : String
    , actionsYes : Maybe String
    , actionsNo : Maybe String
    , date : Date
    , voteEvents : WebData (List VoteEvent)
    }


type VoteId
    = VoteId Int


type alias VoteEvent =
    { personId : Int
    , name : String
    , party : Maybe String
    , option : VoteOption
    }


type VoteOption
    = Yes
    | No
    | Both
    | Absent


init : ( Model, Cmd Msg )
init =
    ( { votes = NotAsked
      , graphVoteId = Nothing
      , voteInput = ""
      }
    , getInitialVotes
    )


getInitialVotes : Cmd Msg
getInitialVotes =
    Http.get "/votes" initialVotesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map InitialVotesResponse


getEventsForVote : VoteId -> Cmd Msg
getEventsForVote voteId =
    let
        (VoteId id) =
            voteId

        path =
            "/vote-events/" ++ toString id
    in
    Http.get path (D.list voteEventDecoder)
        |> RemoteData.sendRequest
        |> Cmd.map (VoteEventsResponse voteId)


initialVotesDecoder : D.Decoder Votes
initialVotesDecoder =
    let
        createInitialVotes =
            \( votes, latestVote ) ->
                let
                    -- Every Vote should have a date, but need to filter
                    -- out any which somehow didn't to ensure this.
                    votesWithDates =
                        Maybe.Extra.values votes
                in
                case latestVote of
                    Just latest ->
                        Votes latest.id
                            (createVotesDict votesWithDates
                                |> Dict.insert latest.id latest
                            )
                            |> D.succeed

                    Nothing ->
                        D.fail "Latest vote has no date!"

        createVotesDict =
            \votes ->
                List.map
                    (\vote -> ( vote.id, vote ))
                    votes
                    |> Dict.fromList
    in
    D.map2 (,)
        (D.field "votes" (D.list voteWithoutEventsDecoder))
        (D.field "latestVote" voteWithEventsDecoder)
        |> D.andThen createInitialVotes


voteWithoutEventsDecoder : D.Decoder (Maybe Vote)
voteWithoutEventsDecoder =
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
        (D.field "id" D.int |> D.map VoteId)
        (D.field "policy_title" D.string)
        (D.field "text" D.string)
        (D.field "actions_yes" (D.nullable D.string))
        (D.field "actions_no" (D.nullable D.string))
        (D.field "date" D.string)


voteWithEventsDecoder : D.Decoder (Maybe Vote)
voteWithEventsDecoder =
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
        (D.field "id" D.int |> D.map VoteId)
        (D.field "policy_title" D.string)
        (D.field "text" D.string)
        (D.field "actions_yes" (D.nullable D.string))
        (D.field "actions_no" (D.nullable D.string))
        (D.field "date" D.string)
        (D.field "voteEvents" (D.list voteEventDecoder |> D.map Success))


voteEventDecoder : D.Decoder VoteEvent
voteEventDecoder =
    D.map4 VoteEvent
        (D.field "person_id" D.int)
        (D.field "name" D.string)
        (D.field "party" (D.nullable D.string))
        (D.field "option" voteOptionDecoder)


voteOptionDecoder : D.Decoder VoteOption
voteOptionDecoder =
    D.string
        |> D.andThen
            (\option ->
                case option of
                    "aye" ->
                        D.succeed Yes

                    "tellaye" ->
                        D.succeed Yes

                    "no" ->
                        D.succeed No

                    "tellno" ->
                        D.succeed No

                    "both" ->
                        D.succeed Both

                    "absent" ->
                        D.succeed Absent

                    _ ->
                        D.fail ("Unknown vote option: " ++ option)
            )



---- UPDATE ----


type Msg
    = InitialVotesResponse (WebData Votes)
    | VoteEventsResponse VoteId (WebData (List VoteEvent))
    | VoteChanged String
    | ShowVote VoteId


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


{-|

    Handle in a standard way updating model and making HTTP requests/sending
    graph data through port, when either selected vote or Votes data changes.

-}
handleVoteStateChange : Model -> ( Model, Cmd Msg )
handleVoteStateChange model =
    case model.votes of
        Success votes ->
            case selectedVote votes of
                Just vote ->
                    case vote.voteEvents of
                        Success voteEvents ->
                            ( { model | graphVoteId = Just vote.id }
                            , sendGraphData vote
                            )

                        NotAsked ->
                            model ! [ getEventsForVote vote.id ]

                        Failure _ ->
                            -- XXX Handle this somewhere?
                            model ! []

                        Loading ->
                            -- XXX Handle this somewhere?
                            model ! []

                Nothing ->
                    model ! []

        _ ->
            model ! []


selectedVote : Votes -> Maybe Vote
selectedVote { selected, data } =
    Dict.get selected data


sendGraphData : Vote -> Cmd msg
sendGraphData vote =
    graphDataValue vote |> graphData


timeOrderedVotes : Votes -> Maybe (SelectList Vote)
timeOrderedVotes { selected, data } =
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



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.votes of
        Success votes ->
            case timeOrderedVotes votes of
                Just votes_ ->
                    viewVotes votes_

                Nothing ->
                    div [] [ text "No votes available." ]

        Failure error ->
            div [] [ "Error loading data: " ++ toString error |> text ]

        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]


viewVotes : SelectList Vote -> Html Msg
viewVotes votes =
    let
        currentVote =
            SelectList.selected votes

        previousVote =
            SelectList.before votes
                |> List.reverse
                |> List.head

        nextVote =
            SelectList.after votes
                |> List.head

        previousVoteButton =
            voteNavigationButton previousVote "<"

        nextVoteButton =
            voteNavigationButton nextVote ">"

        voteNavigationButton =
            \maybeVote ->
                \icon ->
                    case maybeVote of
                        Just { id } ->
                            button [ onClick (ShowVote id) ] [ text icon ]

                        Nothing ->
                            span [] []
    in
    div []
        [ div []
            [ "Current vote: "
                ++ currentVote.policyTitle
                ++ " | "
                ++ currentVote.text
                ++ " | "
                ++ Date.Extra.toFormattedString "ddd MMMM, y" currentVote.date
                |> text
            ]
        , previousVoteButton
        , nextVoteButton
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
