module Main exposing (main)

{-| A simple application demonstrating how (and why) to use Requested.

This program shows a hypothetical email application showing several
"folders" of email. Only one folder is shown at one time. Changing the
folder is possible by clicking on the folder name -- this initiates a
new request for that data. We use Requested to track the mailbox.

All the data is hard-coded, but to make it easier to see what's going
on, requests delay for a random number of seconds between 1 and 10. We
also display the complete contents of the Requested in the view.

Styling is left as an exercise for the reader.

-}

import Browser
import Debug
import Html exposing (Html, a, div, h1, input, label, li, span, text, ul)
import Html.Attributes as Attributes
import Html.Events as Events
import Requested exposing (Requested(..))


{-| This simplified mailbox application only treats messages as
strings, and not even very interesting strings honestly.
-}
type alias Message =
    String


{-| Categories are referred to by name, not by ID or using an enum or anything.
-}
type alias CategoryName =
    String


type alias Mailbox =
    List Message


{-| The type of the "tracker", which we use to identify requests.

We choose this according to our application's needs. Here we choose to
track a "request number" as well as the category for which the request
was made. This lets us distinguish two requests for the same
CategoryName. We could have used CategoryName given the simplifying
assumption that two requests for the same CategoryName are likely to
give the same information.

The name "tracker" comes from elm/http, but in that package a
"tracker" can only ever be a String. We extend the meaning a little
bit here to allow any type (at your discretion).

-}
type alias Tracker =
    ( Int, CategoryName )


type alias Error =
    String


type alias Model =
    -- The data, or at least where the data goes.
    { mailbox : Requested Tracker Error Mailbox

    -- The currently selected category.
    , selectedCategory : CategoryName

    -- The count of requests we've made. We use this in the tracker.
    , requestCount : Int
    }


type Msg
    = ShowCategory CategoryName
    | GotCategory Tracker CategoryName (Result Error Mailbox)


main : Program () Model Msg
main =
    Browser.element
        { init = initial
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


initial : flags -> ( Model, Cmd Msg )
initial _ =
    let
        initialCategory =
            "news"

        tracker =
            ( 0, initialCategory )
    in
    ( { mailbox = Requested.fromTracker tracker
      , selectedCategory = initialCategory
      , requestCount = 0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ h1 [] [ Html.text "Rmail" ]
        , Html.p []
            [ Html.text "This mail \"application\" lets you read all your mail! "
            , Html.em [] [ Html.text "Powered by Requested." ]
            ]
        , displayRequested model.mailbox
        , displayStatus model.mailbox
        , Html.div
            [ Attributes.style "display" "flex" ]
            [ categoriesList model.selectedCategory
            , mailboxDisplay model
            ]
        ]


displayRequested : Requested Tracker Error Mailbox -> Html msg
displayRequested r =
    let
        identifyMailbox mailbox =
            if mailbox == cats then
                "cats"

            else if mailbox == elm then
                "elm"

            else if mailbox == work then
                "work"

            else if mailbox == news then
                "news"

            else
                Debug.todo "unknown mailbox"
    in
    case r of
        Succeeded mailbox ->
            Html.div []
                [ Html.text "Succeeded"
                , Html.ul []
                    [ Html.li [] [ Html.text <| "Mailbox: <<" ++ identifyMailbox mailbox ++ ">>" ]
                    ]
                ]

        Failed err lastSuccess ->
            Html.div []
                [ Html.text "Failed"
                , Html.ul []
                    [ Html.li [] [ Html.text "Error: ", Html.em [] [ Html.text err ] ]
                    , Html.li []
                        [ case lastSuccess of
                            Nothing ->
                                Html.text "No previous mailbox"

                            Just mailbox ->
                                Html.text <| "Was mailbox: <<" ++ identifyMailbox mailbox ++ ">>"
                        ]
                    ]
                ]

        Outstanding tracker lastErr lastSuccess ->
            Html.div []
                [ Html.text "Outstanding"
                , Html.ul []
                    [ Html.li [] [ Html.text <| "Tracker: " ++ Debug.toString tracker ]
                    , Html.li []
                        [ case lastErr of
                            Nothing ->
                                Html.text "No previous error"

                            Just err ->
                                Html.text <| "Was error: " ++ err
                        ]
                    , Html.li []
                        [ case lastSuccess of
                            Nothing ->
                                Html.text "No previous mailbox"

                            Just mailbox ->
                                Html.text <| "Was mailbox: <<" ++ identifyMailbox mailbox ++ ">>"
                        ]
                    ]
                ]


{-| Display a brief banner at the top of the mailbox summarizing
what's going on.
-}
displayStatus : Requested Tracker Error Mailbox -> Html msg
displayStatus r =
    Html.p []
        [ Html.text <|
            case r of
                Outstanding _ Nothing _ ->
                    "Loading..."

                Failed err _ ->
                    "Couldn't load mailbox because: " ++ err

                Outstanding _ (Just err) _ ->
                    "Trying again..."

                Succeeded _ ->
                    ""
        ]


categoriesList : CategoryName -> Html Msg
categoriesList current =
    let
        category name =
            let
                style =
                    if name == current then
                        [ Attributes.style "font-weight" "bold" ]

                    else
                        []
            in
            Html.li style
                [ Html.a
                    [ Attributes.href <| "#" ++ name
                    , Events.onClick (ShowCategory name)
                    ]
                    [ Html.text name ]
                ]
    in
    Html.ul
        [ Attributes.style "border" "1px solid black"
        , Attributes.style "padding" "20px"
        , Attributes.style "margin-right" "20px"
        ]
        [ category "cats"
        , category "elm"
        , category "work"
        , category "news"
        ]


{-| Display the "mailbox" panel on the right side.

According to our UX design, this should show the messages in the last
successfully loaded mailbox.

If we have never loaded a mailbox, we should show the last
received error message.

If we have never received either a success or a failer, we are in the
initial state and should just show a simple "loading"
message.

-}
mailboxDisplay : Model -> Html a
mailboxDisplay model =
    let
        displayMessages messages =
            Html.ul [] <|
                List.map (\message -> Html.li [] [ Html.text message ]) messages

        displayErr err =
            Html.p []
                [ Html.text "There was an error fetching this mail: "
                , Html.em [] [ Html.text err ]
                ]
    in
    Html.div []
        [ Html.p [] [ Html.em [] [ Html.text <| "Messages in category " ++ model.selectedCategory ] ]

        -- We do a pattern match here to emphasize exhaustiveness
        -- (something *must* be displayed here) and to allow more
        -- flexibility (display onionskin, but only over previous
        -- results), but you might instead to prefer to express this
        -- using `getData` and `getError`.
        , case model.mailbox of
            Outstanding _ _ (Just messages) ->
                -- We might also overlay a loading state here.
                displayMessages messages

            Outstanding _ (Just err) Nothing ->
                displayErr err

            Outstanding t Nothing Nothing ->
                Html.text "Loading. Please wait..."

            Failed _ (Just messages) ->
                -- Our UX design doesn't require an error to be
                -- displayed here -- it will show in the banner.
                displayMessages messages

            Failed err Nothing ->
                displayErr err

            Succeeded messages ->
                displayMessages messages
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowCategory categoryName ->
            ( model, Cmd.none )

        GotCategory tracker categoryName response ->
            ( model, Cmd.none )



-- Hardcoded data


cats : Mailbox
cats =
    [ "OMG check out this cute kitty!!"
    , "Cats gonna cat"
    , "Get off the table!!"
    ]


elm : Mailbox
elm =
    [ "Make invalid states unrepresentable"
    , "Types as Sets"
    , "SPA 4 lyfe"
    ]


work : Mailbox
work =
    [ "TPS reports due by EOD today"
    , "Please update to address critical vulnerability in flow-bin 0.99.2"
    , "Re: Agenda for previous meeting"
    ]


news : Mailbox
news =
    [ "15 Shiny Things You Should Buy"
    , "Could This Political Mishap Derail Their Careers?"
    , "Youngsters Are Doing A New Thing. Should They?"
    ]