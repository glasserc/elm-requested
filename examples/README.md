To run this example, use this command:

`elm-live src/Main.elm --start-page=custom.html -- --debug --output=elm.js`

Load it up and try toggling a bunch of folders quickly. You should see
the status of the Requested get updated for each new click (the
tracker should get updated). Additionally, you should see the mailbox
get updated for each success (in chronological order).

Each response gets a random delay from 4 to 10 seconds. If the
response to your last request comes in before others, the other ones
will be discarded, but this may be hard to tell without looking at the
time-travel debugger.
