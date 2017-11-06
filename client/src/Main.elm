port module Main exposing (..)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as D
import Json.Encode as E
import RemoteData exposing (RemoteData(..), WebData)


-- PORTS --


port graphData : E.Value -> Cmd msg


graphDataValue : Vote -> E.Value
graphDataValue vote =
    E.list (List.map voteEventValue vote.votes)


voteEventValue : VoteEvent -> E.Value
voteEventValue event =
    E.object
        [ ( "name", E.string event.name )
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
    { latestVote : WebData Vote }


type alias Vote =
    { policyTitle : String
    , text : String
    , actionsYes : Maybe String
    , actionsNo : Maybe String
    , date : String
    , votes : List VoteEvent
    }


type alias VoteEvent =
    { name : String
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
    ( { latestVote = NotAsked }
    , getLatestVote
    )


getLatestVote : Cmd Msg
getLatestVote =
    Http.get "/latest-vote" decodeVote
        |> RemoteData.sendRequest
        |> Cmd.map VoteResponse


decodeVote : D.Decoder Vote
decodeVote =
    D.map6 Vote
        (D.field "policy_title" D.string)
        (D.field "text" D.string)
        (D.field "actions_yes" (D.nullable D.string))
        (D.field "actions_no" (D.nullable D.string))
        (D.field "date" D.string)
        (D.field "votes" (D.list voteEventDecoder))


voteEventDecoder : D.Decoder VoteEvent
voteEventDecoder =
    D.map3 VoteEvent
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
    = VoteResponse (WebData Vote)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VoteResponse response ->
            ( { model | latestVote = response }
            , sendGraphData response
            )


sendGraphData : WebData Vote -> Cmd msg
sendGraphData voteResponse =
    case voteResponse of
        Success vote ->
            graphDataValue vote |> graphData

        _ ->
            Cmd.none



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , div [] [ text "Your Elm App is working!" ]
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
