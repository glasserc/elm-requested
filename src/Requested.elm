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

  - an outstanding request
  - a failed response
  - a successful response

"At least one" indicates that some previous results are retained:

  - an outstanding request retains both the last failure and the last
    success
  - a failure retains the last success

-}
type Requested t e a
    = Succeeded a
    | Failed e (Maybe a)
    | Outstanding t (Maybe e) (Maybe a)


{-| Utility function to construct a Requested when you already have
the data it would fetch.
-}
fromSuccess : a -> Requested t e a
fromSuccess a =
    Succeeded a


{-| Utility function to construct a Requested when you already have a
failure.

This lets you make lemonade, even when life gives you lemons.

This function is provided for completeness, but generally isn't that
useful.

-}
fromFailure : e -> Requested t e a
fromFailure e =
    Failed e Nothing


{-| Construct a Requested from an initial request.

This is the most common way of creating a Requested.

-}
fromTracker : t -> Requested t e a
fromTracker t =
    Outstanding t Nothing Nothing


{-| Utility function to create a Requested given a Result.

This is provided for completeness.

-}
fromResult : Result e a -> Requested t e a
fromResult r =
    case r of
        Err e ->
            fromFailure e

        Ok a ->
            fromSuccess a


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

If this Requested is not waiting for any data, then ignore the
provided data.

If this Requested _is_ waiting for a request, and this request matches
the given tracker, then produce a new non-waiting Requested.

If this Requested is waiting for a _different_ request, the provided
data probably represents a response from an older request. Update the
Requested to use this data, but remain in a "waiting" state.

FIXME: some use cases might not want to incorporate data from old
requests. Provide another function for them.

-}
withResponse : t -> Result e a -> Requested t e a -> Requested t e a
withResponse responseTracker responseData r =
    case r of
        Outstanding t lastFail lastSuccess ->
            if t == responseTracker then
                case responseData of
                    Ok a ->
                        Succeeded a

                    Err e ->
                        Failed e lastSuccess

            else
                -- This is from another (older?) request.
                -- Incorporate its data.
                case responseData of
                    Ok a ->
                        Outstanding t lastFail (Just a)

                    Err e ->
                        Outstanding t (Just e) lastSuccess

        old ->
            -- We weren't expecting a response. By definition this
            -- must be from a different (outdated) request. Ignore it.
            old


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
getSuccess : Requested t e a -> Maybe a
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
getFailure : Requested t e a -> Maybe e
getFailure r =
    case r of
        Succeeded _ ->
            Nothing

        Failed f _ ->
            Just f

        Outstanding _ lastFailed _ ->
            lastFailed
