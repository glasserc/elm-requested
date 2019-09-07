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
import Process
import Random
import Requested exposing (Requested(..))
import Task


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
give the same information. Alternately, if we were only ever going to
make one request, we could use the unit type () as the tracker.

The name "tracker" comes from elm/http, but in that package a
"tracker" can only ever be a String. We extend the meaning a little
bit here to allow any type (at your discretion).

-}
type alias Tracker =
    ( Int, CategoryName )


{-| The type of the "errors" we could get when trying to make a
request.

We don't want this example to bog down in the discussion of networks
and their failures, so we'll just assume they can fail with String
errors. To keep the Error type from being another alias for String,
we'll also add the possibility of a "no such category" error
response.

What you consider an error and what you consider a successful error
result are completely up to you! In particular, it might make more
sense for NoSuchCategory to be part of a succeeded result, depending
on your types or UX. In this example, we can tell that the category
doesn't even exist before we even "execute" the request, so we could
even decide not to transition to Outstanding, in which case it makes
no sense for this to be part of the error type. However, let's assume
that this mail client is looking a shared mailbox that could be
updated externally, in which case the category might disappear under
our noses.

-}
type Error
    = HttpError String
    | NoSuchCategory String


{-| Get an Int from JS to seed the random request delays.
-}
type alias Flags =
    Int


errToString : Error -> String
errToString err =
    case err of
        HttpError s ->
            "network failure (" ++ s ++ ")"

        NoSuchCategory name ->
            "the category " ++ name ++ " does not exist"


type alias Model =
    -- The data, or at least where the data goes.
    { mailbox : Requested Tracker Error Mailbox

    -- The currently selected category.
    , selectedCategory : CategoryName

    -- The count of requests we've made. We use this in the tracker.
    , requestCount : Int

    -- A random seed that we use to randomize how long requests will take.
    , seed : Random.Seed
    }


type Msg
    = ShowCategory CategoryName
    | GotCategory Tracker CategoryName (Result Error Mailbox)


main : Program Flags Model Msg
main =
    Browser.element
        { init = initial
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


initial : Flags -> ( Model, Cmd Msg )
initial seed =
    let
        initialCategory =
            "news"

        initialTracker =
            ( 0, initialCategory )

        ( initialSeed, initialRequest ) =
            makeRequest (Random.initialSeed seed) initialCategory initialTracker
    in
    ( { mailbox = Requested.fromTracker initialTracker
      , selectedCategory = initialCategory
      , requestCount = 0
      , seed = initialSeed
      }
    , initialRequest
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowCategory categoryName ->
            fetch model categoryName

        GotCategory tracker categoryName response ->
            ( { model
                | mailbox = Requested.withResponse tracker response model.mailbox
              }
            , Cmd.none
            )


fetch : Model -> CategoryName -> ( Model, Cmd Msg )
fetch model categoryName =
    let
        tracker =
            ( model.requestCount, categoryName )

        ( newSeed, cmd ) =
            makeRequest model.seed categoryName tracker
    in
    ( { model
        | requestCount = model.requestCount + 1
        , seed = newSeed
        , mailbox = Requested.refresh tracker model.mailbox
      }
    , cmd
    )


{-| Issue a request for a mailbox. which will resolve after a certain
amount of time. To make things interesting, fail a certain proportion
of the requests.

Strictly speaking, this could be part of `fetch`, but we separate it
out here 1. because it's complicated enough to stand on its own and

1.  because folding it into fetch requires a more complicated
    definition of the `initialModel`.

-}
makeRequest : Random.Seed -> CategoryName -> Tracker -> ( Random.Seed, Cmd Msg )
makeRequest seed categoryName tracker =
    let
        mailboxM =
            case categoryName of
                "cats" ->
                    Just cats

                "elm" ->
                    Just elm

                "work" ->
                    Just work

                "news" ->
                    Just news

                _ ->
                    Nothing

        errorMsgGenerator =
            Random.uniform "Guru meditation number"
                [ "Segmentation fault"
                , "No shared cipher"
                , "NS_ERROR_FAILURE"
                , "PC LOAD LETTER"
                , "lp0 on fire"
                ]

        failChance =
            0.4

        failGenerator =
            errorMsgGenerator
                |> Random.andThen
                    (\error ->
                        Random.weighted ( failChance, Just error )
                            [ ( 1 - failChance, Nothing ) ]
                    )

        delayGenerator =
            Random.float 4 10

        ( ( delay, failure ), nextSeed ) =
            Random.step (Random.pair delayGenerator failGenerator) seed

        naturalResponse =
            case mailboxM of
                Nothing ->
                    Err <| NoSuchCategory categoryName

                Just mailbox ->
                    Ok mailbox

        maybeFail _ =
            case failure of
                Just err ->
                    Err <| HttpError err

                Nothing ->
                    naturalResponse

        waitAndRespond =
            Process.sleep (delay * 1000) |> Task.map maybeFail
    in
    ( nextSeed, Task.perform (GotCategory tracker categoryName) waitAndRespond )


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
                    [ Html.li [] [ Html.text "Error: ", Html.em [] [ Html.text <| Debug.toString err ] ]
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
                                Html.text <| "Was error: " ++ Debug.toString err
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
                    "Couldn't load mailbox because: " ++ errToString err

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
                    -- In a real application, we'd like, try to
                    -- route this or something
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
                , Html.em []
                    [ Html.text <| errToString err ]
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
