module View.VoteEvent exposing (Config, maybeInfoBox)

import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra
import Tachyons exposing (classes)
import Tachyons.Classes as TC exposing (..)
import Tagged
import VoteEvent exposing (VoteEvent)


type alias Config msg =
    { clearSelectedPersonMsg : msg
    , showIcons : Bool
    }


maybeInfoBox : Config msg -> Maybe VoteEvent -> Maybe (Html msg)
maybeInfoBox config =
    Maybe.map (infoBox config)


infoBox : Config msg -> VoteEvent -> Html msg
infoBox { clearSelectedPersonMsg, showIcons } event =
    let
        textColour =
            VoteEvent.partyComplementaryColour event

        lockIcon =
            iconButton left_0
                FeatherIcons.lock
                [ title "Stop tracking"
                , onClick clearSelectedPersonMsg
                ]

        infoLinkIcon =
            iconButton right_0
                FeatherIcons.externalLink
                [ title "View on TheyWorkForYou"
                , href infoLink
                ]

        infoLink =
            "https://www.theyworkforyou.com/mp/" ++ personIdString event

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
                    , personIdString event
                    , suffix
                    ]
    in
    img
        [ src primaryImageUrl
        , height 100
        , attribute "onerror" imageOnError
        ]
        []


personIdString : VoteEvent -> String
personIdString event =
    Tagged.untag event.personId |> toString
