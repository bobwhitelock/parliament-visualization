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
import Keyboard
import Maybe.Extra
import Policy
import RemoteData exposing (RemoteData(..), WebData)
import Select
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
    , personSelectState : Select.State
    , config : Config
    }


type alias Config =
    { apiUrl : String
    }


init : Config -> ( Model, Cmd Msg )
init config =
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
    , personSelectState = Select.newState "personSelect"
    , config = config
    }
        ! [ getInitialData config
          , Cmd.map DatePickerMsg datePickerCmd
          ]


getInitialData : Config -> Cmd Msg
getInitialData config =
    let
        url =
            config.apiUrl ++ "/initial-data"
    in
    Http.get url Votes.decoder
        |> RemoteData.sendRequest
        |> Cmd.map InitialDataResponse



---- UPDATE ----


type Msg
    = InitialDataResponse (WebData Votes)
    | VoteEventsResponse Vote.Id (WebData (List VoteEvent))
    | VoteChanged String
    | ShowVote Vote.Id
    | KeyPress Int
    | PersonNodeHovered Int
    | PersonNodeUnhovered Int
    | PersonNodeClicked Int
    | ClearSelectedPerson
    | ChartSettled ()
    | FilterByPolicy Policy.Id
    | ClearPolicyFilter
    | SelectPerson (Maybe VoteEvent)
    | PersonSelectMsg (Select.Msg VoteEvent)
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
            showVote model newVoteId

        KeyPress keyCode ->
            case model.votes of
                Success votes ->
                    let
                        maybeShowVote =
                            Maybe.map (.id >> showVote model)
                                >> Maybe.withDefault (model ! [])

                        { previous, next } =
                            Votes.neighbouringVotes model.filteredPolicyId votes
                    in
                    case keyCode of
                        -- Left arrow.
                        37 ->
                            maybeShowVote previous

                        -- Right arrow.
                        39 ->
                            maybeShowVote next

                        _ ->
                            model ! []

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
            selectPerson model personId

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

        SelectPerson maybeVoteEvent ->
            case maybeVoteEvent of
                Just voteEvent ->
                    selectPerson model voteEvent.personId

                Nothing ->
                    model ! []

        PersonSelectMsg msg ->
            let
                ( newSelectState, cmd ) =
                    Select.update personSelectConfig msg model.personSelectState
            in
            { model | personSelectState = newSelectState } ! [ cmd ]

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


showVote : Model -> Vote.Id -> ( Model, Cmd Msg )
showVote model voteId =
    case model.votes of
        Success { data, policies } ->
            let
                newVotes =
                    Votes voteId data policies |> Success

                newModel =
                    { model | votes = newVotes }
            in
            handleVoteStateChangeWithRestart newModel

        _ ->
            model ! []


selectPerson : Model -> Int -> ( Model, Cmd Msg )
selectPerson model personId =
    { model | selectedPersonId = Just personId }
        |> handleVoteStateChangeWithoutRestart


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
                            newModel ! [ getEventsForVote model.config vote.id ]

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
                                Maybe.map (cmdToGetEvents model.config) maybeVote
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


cmdToGetEvents : Config -> Vote -> Maybe (Cmd Msg)
cmdToGetEvents config vote =
    if RemoteData.isNotAsked vote.voteEvents then
        getEventsForVote config vote.id |> Just
    else
        Nothing


getEventsForVote : Config -> Vote.Id -> Cmd Msg
getEventsForVote config voteId =
    let
        (Vote.Id id) =
            voteId

        url =
            config.apiUrl ++ "/vote-events/" ++ toString id
    in
    Http.get url (D.list VoteEvent.decoder)
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
    case votes of
        Success votes_ ->
            let
                defaultSettings =
                    DatePicker.defaultSettings

                isDisabled =
                    Votes.filteredVotesOnDate filteredPolicyId votes_
                        >> List.isEmpty

                ( firstYear, lastYear ) =
                    Votes.firstAndLastVoteYears filteredPolicyId votes_
            in
            { defaultSettings
                | isDisabled = isDisabled
                , dateFormatter = Date.Extra.toFormattedString "ddd MMMM, y"
                , inputClassList = [ ( w_100, True ), ( pa1, True ) ]
                , changeYear = DatePicker.between firstYear lastYear
            }

        _ ->
            DatePicker.defaultSettings


personSelectConfig : Select.Config Msg VoteEvent
personSelectConfig =
    let
        classes =
            String.join " "
    in
    Select.newConfig SelectPerson .name
        |> Select.withCutoff 12
        |> Select.withInputWrapperClass mb1
        |> (Select.withInputClass <| classes [ pa1, border_box ])
        |> Select.withItemHtml personSelectItem
        |> (Select.withMenuClass <| classes [ ba, b__gray, bg_white, w_100 ])
        |> Select.withNotFound "No matches"
        |> Select.withNotFoundClass red
        |> Select.withHighlightedItemClass o_50
        -- Hide clear button; not needed.
        |> Select.withClearClass dn
        |> Select.withPrompt "Enter MP to track"



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.votes of
        Success votes ->
            div [] [ page votes model ]

        Failure error ->
            div [] [ "Error loading data: " ++ toString error |> text ]

        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]


page : Votes -> Model -> Html Msg
page votes model =
    case Votes.selected votes of
        Just current ->
            div
                [ classes
                    [ min_vh_100
                    , mw9
                    , bg_near_white
                    , center
                    , flex
                    , flex_column
                    ]
                ]
                [ tachyons.css
                , visualization current votes model
                , pageFooter
                ]

        _ ->
            div [] [ text "No votes available." ]


visualization : Vote -> Votes -> Model -> Html Msg
visualization currentVote votes model =
    let
        { hoveredPersonId, selectedPersonId, filteredPolicyId } =
            model

        currentEventForPersonId =
            Vote.eventForPersonId currentVote

        hoveredPersonEvent =
            currentEventForPersonId hoveredPersonId

        selectedPersonEvent =
            currentEventForPersonId selectedPersonId

        neighbouringVotes =
            Votes.neighbouringVotes filteredPolicyId votes

        datePicker =
            DatePicker.view
                (Just currentVote.date)
                (datePickerSettings model)
                model.datePicker
                |> Html.map DatePickerMsg
                |> Just
    in
    section
        [ classes
            [ pa3
            , ph5_ns
            , helvetica
            , lh_copy
            , f4
            , overflow_hidden
            , flex_auto
            ]
        ]
        [ div
            [ classes [ fl, w_75 ] ]
            [ currentVoteInfo filteredPolicyId votes currentVote
            , nodeHoveredText hoveredPersonId selectedPersonId
            , voteChart currentVote
            ]
        , div [ classes [ fl, w_25 ] ]
            [ div [ classes [ fr, w5 ] ]
                (Maybe.Extra.values
                    [ datePicker
                    , navigationButtons neighbouringVotes |> Just
                    , personSelect model currentVote selectedPersonEvent |> Just
                    , selectedPersonInfoBox selectedPersonEvent
                    , hoveredPersonInfoBox hoveredPersonEvent
                    ]
                )
            ]
        ]


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
            \maybeVote icon relationText ->
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
    div [ classes [ mt1, mb3 ] ]
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
        textColour =
            VoteEvent.partyComplementaryColour event

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
            \position icon attributes ->
                if showIcons then
                    Just
                        (a
                            ([ classes
                                [ absolute
                                , position
                                , bottom_0
                                , pa2
                                , dim
                                , textColour
                                , pointer
                                ]
                             , style [ ( "color", textColour ) ]
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
            , mb1
            ]
        , style [ ( "color", textColour ) ]
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


personSelect : Model -> Vote -> Maybe VoteEvent -> Html Msg
personSelect { personSelectState, selectedPersonId } currentVote selectedPersonEvent =
    let
        voteEvents =
            case currentVote.voteEvents of
                Success voteEvents ->
                    voteEvents

                _ ->
                    []
    in
    Html.map PersonSelectMsg
        (Select.view
            personSelectConfig
            personSelectState
            voteEvents
            selectedPersonEvent
        )


personSelectItem : VoteEvent -> Html Never
personSelectItem voteEvent =
    let
        backgroundColour =
            VoteEvent.partyColour voteEvent

        textColour =
            VoteEvent.partyComplementaryColour voteEvent
    in
    li
        [ style
            [ ( "background-color", backgroundColour )
            , ( "color", textColour )
            ]
        , classes
            [ pa1, bb, b__black, dim, f5, TC.list ]
        ]
        [ text voteEvent.name ]


pageFooter : Html msg
pageFooter =
    footer [ classes [ pv2, ph3, ph5_m, ph6_l ] ]
        [ div [ classes [ tc, mt3 ] ]
            (List.intersperse
                (text "·")
                [ footerLink "Built by Bob Whitelock" "https://github.com/bobwhitelock"
                , footerLink "Source on GitHub" "https://github.com/bobwhitelock/parliament-visualization"
                , footerLink "Data from TheyWorkForYou" "https://www.theyworkforyou.com"
                ]
            )
        ]


footerLink : String -> String -> Html msg
footerLink name url =
    a
        [ classes [ f6, dib, ph2, link, mid_gray, dim ]
        , href url
        , title name
        ]
        [ text name ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ personNodeHovered PersonNodeHovered
        , personNodeUnhovered PersonNodeUnhovered
        , personNodeClicked PersonNodeClicked
        , chartSettled ChartSettled
        , Keyboard.ups KeyPress
        ]



---- PROGRAM ----


main : Program Config Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
