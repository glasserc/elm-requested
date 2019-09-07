# elm-requested: an opinionated library for managing requests

This library introduces a type called `Requested` which is designed
for managing the state of asynchronous, fallible operations,
especially those which can be performed several times. A
`Requested t e a` represents a successful `a`, a failed `e`, or an outstanding
request (identified by `t`) which we expect to eventually resolve into a success or
failure.

A good use case for `Requested` is something like Gmail's main
view. This view shows a bunch of email threads which are fetched from
a remote service. The view can show email threads from your "inbox" as
well as for different "labels". The user can easily switch from label
to label or label to inbox; when this happens, data is requested from
the remote service, and it can take a variable amount of time to
respond. Using `Requested` helps you ensure that:

- If a request fails, it doesn't "erase" previously fetched data.

- We continue to show the existing data while waiting for a new request.

- If the user requests A and then B, we stop "loading" once we get B,
  and we disregard A.

- If the user requests A, then B, then C, and we get A or B, we show
  it, but continue to wait for C.

For an example of this paradigm, check the `examples` directory.

## This sounds like RemoteData

`Requested` is inspired by [RemoteData](https://package.elm-lang.org/packages/krisajenkins/remotedata/),
but with some differences:

- There's no "initializing", "not requested" state. This is because
  many uses of `Requested` correspond to data which is requested
  immediately and for which "not requested" would be an error.

  If you need the additional state of "not requested", this can be easily modeled
  using a `Maybe`. See `Requested.Maybe` for utilities to make this easier.

- `Failed` retains the previous successful result (if
  any). `Outstanding` retains both the previous successful and failed
  result (if any).

- `Outstanding` holds a "tracker" `t` that lets/forces you to identify
  requests and the responses that correspond to them.
