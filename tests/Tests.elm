module Tests exposing (withResponseTests)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Fuzzers exposing (..)
import Requested exposing (Requested(..))
import Test exposing (..)


withResponseTests : Test
withResponseTests =
    let
        someSucceeded =
            succeededFuzzer int int
    in
    describe "withResponseTests"
        [ describe "when Succeeded"
            [ fuzz3 someSucceeded int (result int int) "should ignore everything" <|
                \succeeded t r ->
                    succeeded
                        |> Requested.withResponse compare t r
                        |> Expect.equal succeeded
            ]
        , describe "when Failed"
            [ test "ignore all failed" <|
                \_ ->
                    let
                        failed =
                            Failed ( 1, () ) Nothing
                    in
                    failed
                        |> Requested.withResponse compare 2 (Err ())
                        |> Expect.equal failed
            , test "will adopt first success" <|
                \_ ->
                    Failed ( 1, () ) Nothing
                        |> Requested.withResponse compare 0 (Ok ())
                        |> Expect.equal (Failed ( 1, () ) (Just ( 0, () )))
            , test "will ignore older successes" <|
                \_ ->
                    let
                        failed =
                            Failed ( 10, () ) (Just ( 9, () ))
                    in
                    failed
                        |> Requested.withResponse compare 8 (Ok ())
                        |> Expect.equal failed
            , test "will adopt newer success" <|
                \_ ->
                    Failed ( 10, () ) (Just ( 8, () ))
                        |> Requested.withResponse compare 9 (Ok ())
                        |> Expect.equal (Failed ( 10, () ) (Just ( 9, () )))
            ]
        , let
            noPast =
                Outstanding 10 Nothing Nothing

            succeeded6 =
                Outstanding 10 Nothing (Just ( 6, () ))

            failed6 =
                Outstanding 10 (Just ( 6, () )) Nothing
          in
          describe "when Outstanding"
            [ test "will adopt first success" <|
                \_ ->
                    noPast
                        |> Requested.withResponse compare 8 (Ok ())
                        |> Expect.equal (Outstanding 10 Nothing (Just ( 8, () )))
            , test "will adopt newer success" <|
                \_ ->
                    succeeded6
                        |> Requested.withResponse compare 8 (Ok ())
                        |> Expect.equal (Outstanding 10 Nothing (Just ( 8, () )))
            , test "will ignore older success" <|
                \_ ->
                    succeeded6
                        |> Requested.withResponse compare 5 (Ok ())
                        |> Expect.equal succeeded6
            , test "will adopt first failure" <|
                \_ ->
                    noPast
                        |> Requested.withResponse compare 8 (Err ())
                        |> Expect.equal (Outstanding 10 (Just ( 8, () )) Nothing)
            , test "will adopt newer failure" <|
                \_ ->
                    failed6
                        |> Requested.withResponse compare 8 (Err ())
                        |> Expect.equal (Outstanding 10 (Just ( 8, () )) Nothing)
            , test "will ignore older failure" <|
                \_ ->
                    failed6
                        |> Requested.withResponse compare 5 (Err ())
                        |> Expect.equal failed6
            , test "will ignore success newer than tracked" <|
                \_ ->
                    cases
                        [ noPast, succeeded6, failed6 ]
                    <|
                        \outstanding ->
                            outstanding
                                |> Requested.withResponse compare 11 (Ok ())
                                |> Expect.equal outstanding
            , test "will ignore failure newer than tracked" <|
                \_ ->
                    cases
                        [ noPast, succeeded6, failed6 ]
                    <|
                        \outstanding ->
                            outstanding
                                |> Requested.withResponse compare 11 (Err ())
                                |> Expect.equal outstanding
            , test "converts to Succeeded" <|
                \_ ->
                    cases
                        [ noPast, succeeded6, failed6 ]
                    <|
                        \outstanding ->
                            outstanding
                                |> Requested.withResponse compare 10 (Ok ())
                                |> Expect.equal (Succeeded ( 10, () ))
            , test "converts to Failed with no success" <|
                \_ ->
                    cases
                        [ noPast, failed6 ]
                    <|
                        \outstanding ->
                            outstanding
                                |> Requested.withResponse compare 10 (Err ())
                                |> Expect.equal (Failed ( 10, () ) Nothing)
            , test "converts to Failed with leftover success" <|
                \_ ->
                    succeeded6
                        |> Requested.withResponse compare 10 (Err ())
                        |> Expect.equal (Failed ( 10, () ) (Just ( 6, () )))
            ]
        ]


cases : List a -> (a -> Expectation) -> Expect.Expectation
cases xs f =
    Expect.all
        (List.map (|>) xs)
        f
