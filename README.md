# State machine testing in Haskell using Hedgehog

See presentation in [slides](slides.pdf).

The repo contains accompanying example consisting of:
  - rest application we're testing in [src/App](src/App).
  - state machine test that test this application [test/Spec.hs](test/Spec.hs).

You can run it using `stack run run-app`.
While the app is running you can execute the state machine testing from another terminal
using `stack test`.

# Suggested exercises
- add input validation to create project endpoint (e.g. project name has to be longer than 2 characters and must consist of alphanumeric chars), throwing client error for invalid names
- run the tests and see them fail
- see if you can fix the test by adjusting the commands
    - adjust the project name generator to generate valid inputs only
    - add new command that generates invalid project names and expects appropriate app failure
- generation of project IDs is intentionally buggy. Can you come up with more robust implementation so that it passes parallel tests?
