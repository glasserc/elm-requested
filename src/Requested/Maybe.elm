module Requested.Maybe exposing
    ( notRequested
    , Requested
    , fromTracker, fromSuccess, fromFailure
    , isOutstanding
    , withResponse
    , refresh
    , getSuccess, getFailure
    , fromResult
    )

{-| Pre-composed functions for Maybe Requested.

This module provides the same API as the Requested module, but for
situations where requests haven't necessarily happened yet. For
example, maybe you're changing from a page where a confirmation code
is taken from the URL, to one where it comes from user input in a
form.

Most functions here serve the same purpose as the corresponding ones
in Requested, which see.

@docs notRequested
@docs Requested
@docs fromTracker, fromSuccess, fromFailure
@docs isOutstanding
@docs withResponse
@docs refresh
@docs getSuccess, getFailure
@docs fromResult

-}

import Requested as BaseR


{-| An alias for a Maybe Requested.
-}
type alias Requested t e a =
    Maybe (BaseR.Requested t e a)


{-| A convenience to create a Maybe Requested.
-}
notRequested : Requested t e a
notRequested =
    Nothing


{-| Like Requested.fromTracker, but for a Maybe Requested.
-}
fromTracker : t -> Requested t e a
fromTracker t =
    Just <| BaseR.fromTracker t


{-| Like Requested.fromSuccess, but for a Maybe Requested.
-}
fromSuccess : t -> a -> Requested t e a
fromSuccess t a =
    Just <| BaseR.fromSuccess t a


{-| Like Requested.fromFailure, but for a Maybe Requested.
-}
fromFailure : t -> e -> Requested t e a
fromFailure t e =
    Just <| BaseR.fromFailure t e


{-| Like Requested.isOutstanding, but for a Maybe Requested.
-}
isOutstanding : Requested t e a -> Bool
isOutstanding reqM =
    case reqM of
        Nothing ->
            False

        Just req ->
            BaseR.isOutstanding req


{-| Like Requested.withResponse, but for a Maybe Requested.
-}
withResponse : (t -> t -> Order) -> t -> Result e a -> Requested t e a -> Requested t e a
withResponse compare t resp requestedMaybe =
    case requestedMaybe of
        Nothing ->
            Nothing

        Just requested ->
            Just (BaseR.withResponse compare t resp requested)


{-| Like Requested.refresh, but for a Maybe Requested.
-}
refresh : t -> Requested t e a -> Requested t e a
refresh t requestedMaybe =
    case requestedMaybe of
        Nothing ->
            fromTracker t

        Just requested ->
            Just (BaseR.refresh t requested)


{-| Like Requested.getSuccess, but for a Maybe Requested.
-}
getSuccess : Requested t e a -> Maybe ( t, a )
getSuccess =
    Maybe.andThen BaseR.getSuccess


{-| Like Requested.getFailure, but for a Maybe Requested.
-}
getFailure : Requested t e a -> Maybe ( t, e )
getFailure =
    Maybe.andThen BaseR.getFailure


{-| Like Requested.fromResult, but for a Maybe Requested.
-}
fromResult : t -> Result e a -> Requested t e a
fromResult t r =
    Just (BaseR.fromResult t r)
