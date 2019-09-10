module Fuzzers exposing
    ( failedFuzzer
    , outstandingFuzzer
    , requestedFuzzer
    , result
    , succeededFuzzer
    )

import Fuzz exposing (Fuzzer)
import Requested exposing (Requested(..))


requestedFuzzer : Fuzzer t -> Fuzzer e -> Fuzzer a -> Fuzzer (Requested t e a)
requestedFuzzer fuzzT fuzzE fuzzA =
    Fuzz.oneOf
        [ succeededFuzzer fuzzT fuzzA
        , failedFuzzer fuzzT fuzzE fuzzA
        , outstandingFuzzer fuzzT fuzzE fuzzA
        ]


succeededFuzzer : Fuzzer t -> Fuzzer a -> Fuzzer (Requested t e a)
succeededFuzzer fuzzT fuzzA =
    Fuzz.map Succeeded (Fuzz.tuple ( fuzzT, fuzzA ))


failedFuzzer : Fuzzer t -> Fuzzer e -> Fuzzer a -> Fuzzer (Requested t e a)
failedFuzzer fuzzT fuzzE fuzzA =
    Fuzz.map2 Failed (Fuzz.tuple ( fuzzT, fuzzE )) (Fuzz.maybe <| Fuzz.tuple ( fuzzT, fuzzA ))


outstandingFuzzer : Fuzzer t -> Fuzzer e -> Fuzzer a -> Fuzzer (Requested t e a)
outstandingFuzzer fuzzT fuzzE fuzzA =
    Fuzz.map3 Outstanding fuzzT (Fuzz.maybe <| Fuzz.tuple ( fuzzT, fuzzE )) (Fuzz.maybe <| Fuzz.tuple ( fuzzT, fuzzA ))


result : Fuzzer e -> Fuzzer a -> Fuzzer (Result e a)
result fuzzE fuzzA =
    Fuzz.oneOf
        [ Fuzz.map Err fuzzE
        , Fuzz.map Ok fuzzA
        ]
