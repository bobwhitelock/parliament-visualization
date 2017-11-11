port module Main exposing (..)

import EveryDict as Dict exposing (EveryDict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import RemoteData exposing (RemoteData(..), WebData)


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
    , date : String
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
            \votes ->
                \latestVote ->
                    Votes latestVote.id
                        (createVotesDict votes
                            |> Dict.insert latestVote.id latestVote
                        )

        createVotesDict =
            \votes ->
                List.map
                    (\vote -> ( vote.id, vote ))
                    votes
                    |> Dict.fromList
    in
    D.map2 createInitialVotes
        (D.field "votes" (D.list voteWithoutEventsDecoder))
        (D.field "latestVote" voteWithEventsDecoder)


voteWithoutEventsDecoder : D.Decoder Vote
voteWithoutEventsDecoder =
    let
        initialVoteState =
            \id ->
                \policyTitle ->
                    \text ->
                        \actionsYes ->
                            \actionsNo ->
                                \date ->
                                    Vote
                                        id
                                        policyTitle
                                        text
                                        actionsYes
                                        actionsNo
                                        date
                                        NotAsked
    in
    D.map6 initialVoteState
        (D.field "id" D.int |> D.map VoteId)
        (D.field "policy_title" D.string)
        (D.field "text" D.string)
        (D.field "actions_yes" (D.nullable D.string))
        (D.field "actions_no" (D.nullable D.string))
        (D.field "date" D.string)


voteWithEventsDecoder : D.Decoder Vote
voteWithEventsDecoder =
    -- XXX de-duplicate this and above.
    D.map7 Vote
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
    | RequestVoteEvents
    | VoteEventsResponse VoteId (WebData (List VoteEvent))
    | VoteChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialVotesResponse response ->
            let
                cmd =
                    case response of
                        Success votes ->
                            selectedVote votes
                                |> Maybe.map sendGraphData
                                |> Maybe.withDefault Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | votes = response }, cmd )

        RequestVoteEvents ->
            let
                id =
                    String.toInt model.voteInput
                        |> Result.toMaybe
                        |> Maybe.map VoteId
            in
            case ( id, model.votes ) of
                ( Just id_, Success votes ) ->
                    ( model, getEventsForVote id_ )

                _ ->
                    model ! []

        VoteEventsResponse voteId response ->
            let
                votes =
                    case model.votes of
                        Success { selected, data } ->
                            Votes selected
                                (Dict.update
                                    voteId
                                    (Maybe.map (\vote -> { vote | voteEvents = response }))
                                    data
                                )
                                |> Success

                        -- Initial data didn't load, so shouldn't receive and
                        -- can't do anything with this response.
                        otherWebData ->
                            otherWebData
            in
            { model | votes = votes } ! []

        VoteChanged input ->
            { model | voteInput = input } ! []


selectedVote : Votes -> Maybe Vote
selectedVote { selected, data } =
    Dict.get selected data


sendGraphData : Vote -> Cmd msg
sendGraphData vote =
    graphDataValue vote |> graphData



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit RequestVoteEvents ]
            [ input
                [ onInput VoteChanged
                , placeholder "Enter vote ID to get"
                ]
                []
            , button [] [ text "Request" ]
            ]
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
