port module Main exposing (..)

import Date.Extra
import DatePicker exposing (DatePicker)
import EveryDict as Dict
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra
import Policy
import RemoteData exposing (RemoteData(..), WebData)
import Svg
import Svg.Attributes
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes as TC exposing (..)
import Vote exposing (Vote)
import VoteEvent exposing (VoteEvent)
import Votes exposing (NeighbouringVotes, Votes)


-- PORTS --


port chartData : E.Value -> Cmd msg


port chartSettled : (() -> msg) -> Sub msg


port personNodeHovered : (Int -> msg) -> Sub msg


port personNodeUnhovered : (Int -> msg) -> Sub msg


port personNodeClicked : (Int -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { votes : WebData Votes
    , chartVoteId : Maybe Vote.Id
    , voteInput : String
    , hoveredPersonId : Maybe Int
    , selectedPersonId : Maybe Int
    , filteredPolicyId : Maybe Policy.Id
    , datePicker : DatePicker
    }


init : ( Model, Cmd Msg )
init =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    { votes = NotAsked
    , chartVoteId = Nothing
    , voteInput = ""
    , hoveredPersonId = Nothing
    , selectedPersonId = Nothing
    , filteredPolicyId = Nothing
    , datePicker = datePicker
    }
        ! [ getInitialData
          , Cmd.map DatePickerMsg datePickerCmd
          ]


getInitialData : Cmd Msg
getInitialData =
    Http.get "/initial-data" Votes.decoder
        |> RemoteData.sendRequest
        |> Cmd.map InitialDataResponse



---- UPDATE ----


type Msg
    = InitialDataResponse (WebData Votes)
    | VoteEventsResponse Vote.Id (WebData (List VoteEvent))
    | VoteChanged String
    | ShowVote Vote.Id
    | PersonNodeHovered Int
    | PersonNodeUnhovered Int
    | PersonNodeClicked Int
    | ClearSelectedPerson
    | ChartSettled ()
    | FilterByPolicy Policy.Id
    | ClearPolicyFilter
    | DatePickerMsg DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitialDataResponse votes ->
            let
                currentVote =
                    case votes of
                        Success votes_ ->
                            Votes.selected votes_

                        _ ->
                            Nothing

                pickCurrentVoteDate =
                    Maybe.map .date currentVote
                        |> DatePicker.pick

                -- Need to explicitly set date of current Vote in DatePicker,
                -- otherwise will default to current date which may not have a
                -- Vote on it.
                ( newDatePicker, datePickerCmd, dateEvent ) =
                    DatePicker.update
                        (datePickerSettings model)
                        pickCurrentVoteDate
                        model.datePicker

                datePickerCmd_ =
                    Cmd.map DatePickerMsg datePickerCmd

                ( newModel, initialCmd ) =
                    handleVoteStateChangeWithRestart
                        { model
                            | votes = votes
                            , datePicker = newDatePicker
                        }
            in
            newModel ! [ initialCmd, datePickerCmd_ ]

        VoteEventsResponse voteId response ->
            case model.votes of
                Success { selected, data, policies } ->
                    let
                        newVotes =
                            Votes selected
                                (Dict.update
                                    voteId
                                    (Maybe.map (\vote -> { vote | voteEvents = response }))
                                    data
                                )
                                policies
                                |> Success

                        newModel =
                            { model | votes = newVotes }
                    in
                    handleVoteStateChangeWithRestart newModel

                _ ->
                    model ! []

        VoteChanged input ->
            { model | voteInput = input } ! []

        ShowVote newVoteId ->
            case model.votes of
                Success { data, policies } ->
                    let
                        newVotes =
                            Votes newVoteId data policies |> Success

                        newModel =
                            { model | votes = newVotes }
                    in
                    handleVoteStateChangeWithRestart newModel

                _ ->
                    model ! []

        PersonNodeHovered personId ->
            { model | hoveredPersonId = Just personId } ! []

        PersonNodeUnhovered personId ->
            let
                newModel =
                    if model.hoveredPersonId == Just personId then
                        { model | hoveredPersonId = Nothing }
                    else
                        model
            in
            newModel ! []

        PersonNodeClicked personId ->
            { model | selectedPersonId = Just personId } |> handleVoteStateChangeWithoutRestart

        ClearSelectedPerson ->
            { model | selectedPersonId = Nothing } |> handleVoteStateChangeWithoutRestart

        ChartSettled _ ->
            -- Only request neighbouring vote events, if needed, once we are
            -- informed that the current chart simulation has mostly complete.
            -- If this is done sooner it appears to noticeably interrupt and
            -- slow down the simulation, while waiting for the simulation to
            -- fully complete can take quite a long time; this seems a
            -- reasonable compromise.
            model ! [ getNeighbouringVoteEvents model ]

        FilterByPolicy policyId ->
            { model | filteredPolicyId = Just policyId } ! []

        ClearPolicyFilter ->
            { model | filteredPolicyId = Nothing } ! []

        DatePickerMsg msg ->
            case model.votes of
                Success votes ->
                    let
                        ( newDatePicker, datePickerCmd, dateEvent ) =
                            DatePicker.update
                                (datePickerSettings model)
                                msg
                                model.datePicker

                        newSelectedVoteId =
                            case dateEvent of
                                DatePicker.NoChange ->
                                    Votes.selected votes |> Maybe.map .id

                                DatePicker.Changed newDate ->
                                    Maybe.map
                                        (\date ->
                                            Votes.filteredVotesOnDate
                                                model.filteredPolicyId
                                                votes
                                                date
                                                |> List.head
                                        )
                                        newDate
                                        |> Maybe.Extra.join
                                        |> Maybe.map .id

                        ( newVotes, voteChangedCmd ) =
                            case newSelectedVoteId of
                                Just id ->
                                    ( Success { votes | selected = id }
                                    , handleVoteStateChangeWithRestart
                                    )

                                Nothing ->
                                    ( Success votes
                                    , \model -> ( model, Cmd.none )
                                    )

                        ( newModel, newCmd ) =
                            { model
                                | datePicker = newDatePicker
                                , votes = newVotes
                            }
                                |> voteChangedCmd
                    in
                    newModel
                        ! [ Cmd.map DatePickerMsg datePickerCmd
                          , newCmd
                          ]

                _ ->
                    model ! []


handleVoteStateChangeWithRestart : Model -> ( Model, Cmd Msg )
handleVoteStateChangeWithRestart =
    handleVoteStateChange True


handleVoteStateChangeWithoutRestart : Model -> ( Model, Cmd Msg )
handleVoteStateChangeWithoutRestart =
    handleVoteStateChange False


{-|

    Handle in a standard way updating model and making HTTP requests/sending
    graph data through port, when either selected vote or Votes data changes.

-}
handleVoteStateChange : Bool -> Model -> ( Model, Cmd Msg )
handleVoteStateChange restartSimulation model =
    case model.votes of
        Success votes ->
            case Votes.selected votes of
                Just vote ->
                    case vote.voteEvents of
                        Success voteEvents ->
                            { model | chartVoteId = Just vote.id }
                                ! [ sendChartData restartSimulation model.selectedPersonId vote ]

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
                                    Votes
                                        votes.selected
                                        newVotesData
                                        votes.policies
                                        |> Success

                                newModel =
                                    { model | votes = newVotes }
                            in
                            newModel ! [ getEventsForVote vote.id ]

                        Failure _ ->
                            -- XXX Handle this somewhere?
                            model ! []

                        Loading ->
                            model ! []

                Nothing ->
                    model ! []

        _ ->
            model ! []


getNeighbouringVoteEvents : Model -> Cmd Msg
getNeighbouringVoteEvents model =
    case model.votes of
        Success votes ->
            case Votes.selected votes of
                Just vote ->
                    let
                        { previous, next } =
                            Votes.neighbouringVotes model.filteredPolicyId votes

                        maybeCmdToGet =
                            \maybeVote ->
                                Maybe.map cmdToGetEvents maybeVote
                                    |> Maybe.Extra.join

                        neighbouringVotesToGet =
                            Maybe.Extra.values
                                [ maybeCmdToGet previous, maybeCmdToGet next ]
                    in
                    Cmd.batch neighbouringVotesToGet

                Nothing ->
                    Cmd.none

        _ ->
            Cmd.none


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


sendChartData : Bool -> Maybe Int -> Vote -> Cmd msg
sendChartData restartSimulation selectedPersonId vote =
    encodeChartData restartSimulation selectedPersonId vote |> chartData


encodeChartData : Bool -> Maybe Int -> Vote -> E.Value
encodeChartData restartSimulation selectedPersonId vote =
    let
        voteEvents =
            Vote.encode selectedPersonId vote
    in
    E.object
        [ ( "voteEvents", voteEvents )
        , ( "restartSimulation", E.bool restartSimulation )
        ]


datePickerSettings : Model -> DatePicker.Settings
datePickerSettings { filteredPolicyId, votes } =
    let
        defaultSettings =
            DatePicker.defaultSettings

        isDisabled =
            case votes of
                Success votes_ ->
                    Votes.filteredVotesOnDate filteredPolicyId votes_
                        >> List.isEmpty

                _ ->
                    always True
    in
    { defaultSettings
        | isDisabled = isDisabled
        , dateFormatter = Date.Extra.toFormattedString "ddd MMMM, y"
        , inputClassList = [ ( w_100, True ) ]
    }



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.votes of
        Success votes ->
            viewVotes votes model

        Failure error ->
            div [] [ "Error loading data: " ++ toString error |> text ]

        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]


viewVotes : Votes -> Model -> Html Msg
viewVotes votes model =
    let
        { hoveredPersonId, selectedPersonId, filteredPolicyId } =
            model
    in
    case Votes.selected votes of
        Just current ->
            let
                currentEventForPersonId =
                    Vote.eventForPersonId current

                hoveredPersonEvent =
                    currentEventForPersonId hoveredPersonId

                selectedPersonEvent =
                    currentEventForPersonId selectedPersonId

                neighbouringVotes =
                    Votes.neighbouringVotes filteredPolicyId votes

                datePicker =
                    DatePicker.view
                        (Just current.date)
                        (datePickerSettings model)
                        model.datePicker
                        |> Html.map DatePickerMsg
                        |> Just
            in
            section
                [ classes
                    [ min_vh_100
                    , mw9
                    , center
                    , bg_near_white
                    , pa3
                    , ph5_ns
                    , helvetica
                    , lh_copy
                    , f4
                    , overflow_hidden
                    ]
                ]
                [ tachyons.css
                , div
                    [ classes [ fl, w_75 ] ]
                    [ currentVoteInfo filteredPolicyId votes current
                    , nodeHoveredText hoveredPersonId selectedPersonId
                    , voteChart current
                    ]
                , div [ classes [ fl, w_25 ] ]
                    [ div [ classes [ fr, w5 ] ]
                        (Maybe.Extra.values
                            [ datePicker
                            , navigationButtons neighbouringVotes |> Just
                            , selectedPersonInfoBox selectedPersonEvent
                            , hoveredPersonInfoBox hoveredPersonEvent
                            ]
                        )
                    ]
                ]

        _ ->
            div [] [ text "No votes available." ]


voteChart : Vote -> Html msg
voteChart vote =
    let
        chartClasses =
            if RemoteData.isLoading vote.voteEvents then
                [ o_70 ]
            else
                []

        chart =
            div [ classes chartClasses ]
                [ Svg.svg
                    [ width 1000
                    , height 550
                    , id "d3-simulation"
                    , Svg.Attributes.class "db center"
                    ]
                    []
                ]

        ayeText =
            voteDescription "Aye" vote.actionsYes

        noText =
            voteDescription "No" vote.actionsNo

        absentOrBothText =
            strong [] [ text "Absent or Both" ]
    in
    div
        [ classes [ center, mw_100 ] ]
        [ chart
        , div [ classes [ tc ] ]
            [ span [ classes [ fl, w_40, border_box, pr4 ] ] [ ayeText ]
            , span [ classes [ fl, w_20 ] ] [ absentOrBothText ]
            , span [ classes [ fl, w_40, border_box, pl4 ] ] [ noText ]
            ]
        ]


voteDescription : String -> String -> Html msg
voteDescription vote details =
    let
        details_ =
            String.trim details

        voteHtml =
            strong [] [ text vote ]
    in
    if String.isEmpty details_ then
        voteHtml
    else
        span [] [ voteHtml, " " ++ details_ |> text ]


currentVoteInfo : Maybe Policy.Id -> Votes -> Vote -> Html Msg
currentVoteInfo filteredPolicyId votes currentVote =
    let
        currentVotePolicies =
            List.map
                (\policyId -> Dict.get policyId votes.policies)
                currentVote.policyIds
                |> Maybe.Extra.values

        policyButtons =
            List.map
                (\policy ->
                    let
                        ( colour, msg, titleText ) =
                            if Just policy.id == filteredPolicyId then
                                ( bg_silver
                                , ClearPolicyFilter
                                , "Currently only showing votes related to this policy; click to show all votes"
                                )
                            else
                                ( buttonColour
                                , FilterByPolicy policy.id
                                , "Only show votes related to this policy"
                                )
                    in
                    button
                        [ classes [ colour ]
                        , onClick msg
                        , title titleText
                        ]
                        [ text policy.title ]
                )
                currentVotePolicies
    in
    div
        [ classes [ TC.h4 ] ]
        [ "Current vote: " ++ currentVote.text |> text
        , div [] policyButtons
        ]


nodeHoveredText : Maybe Int -> Maybe Int -> Html Msg
nodeHoveredText hoveredPersonId selectedPersonId =
    let
        hoveredText =
            Maybe.map
                (\id ->
                    if Just id == selectedPersonId then
                        ""
                    else
                        "Click to Track"
                )
                hoveredPersonId
                |> Maybe.withDefault ""
    in
    div
        [ classes [ w_100, TC.h1, tc, gray ]
        ]
        [ text hoveredText ]


navigationButtons : NeighbouringVotes -> Html Msg
navigationButtons { previous, next } =
    let
        previousVoteButton =
            voteNavigationButton
                previous
                FeatherIcons.chevronLeft
                "earlier"

        nextVoteButton =
            voteNavigationButton
                next
                FeatherIcons.chevronRight
                "later"

        voteNavigationButton =
            \maybeVote ->
                \icon ->
                    \relationText ->
                        let
                            titleText =
                                String.join " "
                                    [ "Show"
                                    , relationText
                                    , "vote"
                                    ]
                        in
                        case maybeVote of
                            Just { id } ->
                                button
                                    [ onClick (ShowVote id)
                                    , classes [ w_50, buttonColour ]
                                    , title titleText
                                    ]
                                    [ icon ]

                            Nothing ->
                                -- XXX Just disable button in this case instead?
                                span [] []
    in
    div [ classes [ mb3 ] ]
        [ previousVoteButton
        , nextVoteButton
        ]


buttonColour : String
buttonColour =
    bg_white


selectedPersonInfoBox : Maybe VoteEvent -> Maybe (Html Msg)
selectedPersonInfoBox =
    maybePersonInfoBox True


hoveredPersonInfoBox : Maybe VoteEvent -> Maybe (Html Msg)
hoveredPersonInfoBox =
    maybePersonInfoBox False


maybePersonInfoBox : Bool -> Maybe VoteEvent -> Maybe (Html Msg)
maybePersonInfoBox showIcons =
    Maybe.map (personInfoBox showIcons)


personInfoBox : Bool -> VoteEvent -> Html Msg
personInfoBox showIcons event =
    let
        colour =
            if VoteEvent.isSpeaker event then
                -- Showing speaker's party colour as black, so show text as
                -- white so can see it.
                white
            else
                black

        lockIcon =
            iconButton left_0
                FeatherIcons.lock
                [ title "Stop tracking"
                , onClick ClearSelectedPerson
                ]

        infoLinkIcon =
            iconButton right_0
                FeatherIcons.externalLink
                [ title "View on TheyWorkForYou"
                , href infoLink
                ]

        infoLink =
            "https://www.theyworkforyou.com/mp/" ++ toString event.personId

        iconButton =
            \position ->
                \icon ->
                    \attributes ->
                        if showIcons then
                            Just
                                (a
                                    ([ classes
                                        [ absolute
                                        , position
                                        , bottom_0
                                        , pa2
                                        , dim
                                        , colour
                                        , pointer
                                        ]
                                     , target "_blank"
                                     ]
                                        ++ attributes
                                    )
                                    [ icon ]
                                )
                        else
                            Nothing
    in
    div
        [ classes
            [ br2
            , bg_white
            , TC.h5
            , f3
            , tc
            , relative
            , colour
            , mb1
            ]
        ]
        (Maybe.Extra.values
            [ Just (personInfo event)
            , lockIcon
            , infoLinkIcon
            ]
        )


personInfo : VoteEvent -> Html msg
personInfo event =
    div
        [ classes [ h_100 ]
        , style [ ( "background-color", VoteEvent.partyColour event ) ]
        ]
        [ div [] [ text event.name ]
        , personImage event
        , div [] [ text event.party ]
        ]


personImage : VoteEvent -> Html msg
personImage event =
    let
        primaryImageUrl =
            imageUrl ".jpeg"

        secondaryImageUrl =
            imageUrl ".jpg"

        imageOnError =
            -- For some reason TWFY images mostly have a `jpeg` extension but
            -- sometimes have `jpg`; if the former fails to load then attempt
            -- to load the latter (see
            -- https://stackoverflow.com/a/92819/2620402).
            String.join ""
                [ "this.onerror=null;this.src='"
                , secondaryImageUrl
                , "';"
                ]

        imageUrl =
            \suffix ->
                String.join ""
                    [ "https://www.theyworkforyou.com/images/mps/"
                    , toString event.personId
                    , suffix
                    ]
    in
    img
        [ src primaryImageUrl
        , height 100
        , attribute "onerror" imageOnError
        ]
        []



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ personNodeHovered PersonNodeHovered
        , personNodeUnhovered PersonNodeUnhovered
        , personNodeClicked PersonNodeClicked
        , chartSettled ChartSettled
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
