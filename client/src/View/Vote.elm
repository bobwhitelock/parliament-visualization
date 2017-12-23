module View.Vote exposing (chart, navigationButtons)

import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData
import Svg
import Svg.Attributes
import Tachyons exposing (classes)
import Tachyons.Classes exposing (..)
import View
import Vote exposing (Vote)
import Votes exposing (NeighbouringVotes)


navigationButtons : (Vote.Id -> msg) -> NeighbouringVotes -> Html msg
navigationButtons showVoteMsg { previous, next } =
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
                            [ onClick (showVoteMsg id)
                            , classes [ w_50, View.buttonColour ]
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


chart : Vote -> Html msg
chart vote =
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
            description "Aye" vote.actionsYes

        noText =
            description "No" vote.actionsNo

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


description : String -> String -> Html msg
description vote details =
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
