# Server component for coachassistant

Docker and Fig needs to be installed.

Run the project:

    $ git submodule init && git submodule update
    $ fig up

For rebuilding the project, do the following:

    $ fig kill && fig rm --force && fig build && fig up
