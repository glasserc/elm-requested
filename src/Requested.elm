module Requested exposing
    ( Requested(..)
    , fromTracker, fromSuccess, fromFailure
    , isOutstanding
    , withResponse
    , refresh
    , getSuccess, getFailure
    , fromResult
    )

{-| The Requested type and associated functions.

The Requested type is an opinionated paradigm for managing
asynchronous, fallible operations, particularly those which can happen
several times.

The README gives a good overview. This module houses the Requested
type itself and the functions you probably need to use it.

@docs Requested
@docs fromTracker, fromSuccess, fromFailure
@docs isOutstanding
@docs withResponse
@docs refresh
@docs getSuccess, getFailure
@docs fromResult

-}


{-| The Requested type.

A `Requested t e a` has at least one of:

  - an outstanding request, i.e. "loading"
  - a failed response
  - a successful response

"At least one" indicates that some previous results are retained:

  - an outstanding request retains both the last failure and the last
    success
  - a failure retains the last success

The type t represents a "tracker" which is used to identify
requests. You are required to provide a comparison function to order
trackers. We keep the tracker for each response so that we can handle
out-of-order responses.

The constructors are:

  - `Succeeded`: The latest response was successful. We also keep the
    tracker that produced this successful response, which lets us
    identify newer responses (should we transition to `Failed` or
    `Outstanding`). Additionally, you may find that you want to use
    the tracker to identify the current state in some way.

  - `Failed`: The latest response was a failure. We also keep the last
    success. If successes come in that are newer than that success, we
    will update it. Failures should not come in that are newer than
    the last failure because if there were newer requests, we should
    be in `Outstanding`.

  - `Outstanding`: A request has been issued to which we have not
    received the response yet. That request should always be the
    newest request made for this `Requested`. If we are not in this
    state, then it should be impossible to receive a response later
    than what we have.

    Responses for other requests will be used, if necessary,
    to update the previous failure and previous success states, but we
    will remain "waiting" until we get the response corresponding to
    our current request.

-}
type Requested t e a
    = Succeeded ( t, a )
    | Failed ( t, e ) (Maybe ( t, a ))
    | Outstanding t (Maybe ( t, e )) (Maybe ( t, a ))


{-| Utility function to construct a Requested when you already have
the data it would fetch.
-}
fromSuccess : t -> a -> Requested t e a
fromSuccess t a =
    Succeeded ( t, a )


{-| Utility function to construct a Requested when you already have a
failure.

This lets you make lemonade, even when life gives you lemons.

This function is provided for completeness, but generally isn't that
useful.

-}
fromFailure : t -> e -> Requested t e a
fromFailure t e =
    Failed ( t, e ) Nothing


{-| Construct a Requested from an initial request.

This is the most common way of creating a Requested.

-}
fromTracker : t -> Requested t e a
fromTracker t =
    Outstanding t Nothing Nothing


{-| Utility function to create a Requested given a Result.

This is provided for completeness.

-}
fromResult : t -> Result e a -> Requested t e a
fromResult t r =
    case r of
        Err e ->
            fromFailure t e

        Ok a ->
            fromSuccess t a


{-| Check if a Requested is waiting for a request to complete.

This might be useful when choosing to display a "Loading..." state in
your view.

-}
isOutstanding : Requested t e a -> Bool
isOutstanding r =
    case r of
        Outstanding _ _ _ ->
            True

        Failed _ _ ->
            False

        Succeeded _ ->
            False


{-| Update the Requested with data that came in from the request given
by the tracker.

If this Requested is waiting for a request, and this request matches
the given tracker, then produce a new non-waiting Requested.

If this Requested is waiting for a _different_ request, the provided
data probably represents a response from an older request. Update the
Requested to use this data (if necessary), but remain in a "waiting" state.

Otherwise, the Requested is not waiting for a request. Because
requests are ordered, and we aren't in Outstanding, the response must
be older than what we have, so we will remain in whatever state we are
in (Failed or Succeeded). If we are in Failed, it's possible that this
is a newer success than our "last success", so update that.

N.B. This function is pretty strict about responses being ordered and
being in Outstanding if you have made a new request. We don't check if
a response is newer "just in case" or anything like that because that
would hide bugs. Instead, if your ordering function is screwed up
somehow, or you forgot to call `refresh`, some updates will be
silently ignored.

FIXME: some use cases might not want to incorporate data from old
requests. Provide another function for them.

-}
withResponse : (t -> t -> Order) -> t -> Result e a -> Requested t e a -> Requested t e a
withResponse compare responseTracker responseData r =
    let
        update : Maybe ( t, x ) -> t -> x -> Maybe ( t, x )
        update oldM t x =
            case oldM of
                Nothing ->
                    Just ( t, x )

                Just ( oldT, oldX ) ->
                    case compare oldT t of
                        LT ->
                            Just ( t, x )

                        _ ->
                            oldM
    in
    case r of
        Outstanding t lastFail lastSuccess ->
            case compare responseTracker t of
                EQ ->
                    case responseData of
                        Ok a ->
                            Succeeded ( responseTracker, a )

                        Err e ->
                            Failed ( responseTracker, e ) lastSuccess

                LT ->
                    -- This is from another, older request.
                    -- Incorporate its data.
                    case responseData of
                        Ok a ->
                            Outstanding t lastFail (update lastSuccess responseTracker a)

                        Err e ->
                            Outstanding t (update lastFail responseTracker e) lastSuccess

                GT ->
                    -- This request hasn't happened yet.
                    -- Either the user forgot to call `refresh`, or
                    -- there's a bug with their comparison function.
                    -- We can't help them. We can't even log a
                    -- warning. Just stick with what we have to try to
                    -- let them know that there's a problem.
                    r

        Failed ( t, e ) lastSuccess ->
            case responseData of
                Ok a ->
                    Failed ( t, e ) (update lastSuccess responseTracker a)

                Err _ ->
                    -- We aren't waiting for a response, which means
                    -- this response must be older than the one we
                    -- have, so discard it.
                    r

        Succeeded _ ->
            r


{-| Update this Requested to reflect that we are making a new request.
-}
refresh : t -> Requested t e a -> Requested t e a
refresh t r =
    case r of
        Succeeded a ->
            Outstanding t Nothing (Just a)

        Failed e lastSuccess ->
            Outstanding t (Just e) lastSuccess

        Outstanding _ lastFail lastSuccess ->
            Outstanding t lastFail lastSuccess


{-| Get the previous success from the Requested, if any.

This is a utility function which may be useful in writing views.

-}
getSuccess : Requested t e a -> Maybe ( t, a )
getSuccess r =
    case r of
        Succeeded s ->
            Just s

        Failed _ lastSuccess ->
            lastSuccess

        Outstanding _ _ lastSuccess ->
            lastSuccess


{-| Get the previous failure from the Requested, if any.

This is a utility function which may be useful in writing views.

-}
getFailure : Requested t e a -> Maybe ( t, e )
getFailure r =
    case r of
        Succeeded _ ->
            Nothing

        Failed f _ ->
            Just f

        Outstanding _ lastFailed _ ->
            lastFailed
