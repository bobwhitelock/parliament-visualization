module Main exposing (..)

import Html exposing (Html, div, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as D
import RemoteData exposing (RemoteData(..), WebData)


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
            ( { model | latestVote = response }, Cmd.none )



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
