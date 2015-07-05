# Server component for coachassistant

## Building and running

Docker and Fig needs to be installed.

Run the project:

    $ git submodule init && git submodule update
    $ fig up

For rebuilding the project, do the following:

    $ fig kill && fig rm --force && fig build && fig up

Compiling:

    $ cabal sandbox delete
    $ cabal sandbox init
    $ cabal update
    $ cabal sandbox add-source ./lambdatrade-common
    $ cabal install

## API example using Curl

Let's start by adding a club:

    curl --data '{ "name": "Bergsjö IF" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs

Note that the service generates a UUID identifier for the club in the response:

    {"uuid":"5f7e5a88-68bc-4581-8103-c8b2effc373d","name":"Bergsjö IF"}

We will use this identifier to below.

First, let's add two teams.

    curl --data '{ "name": "P05" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/teams
    curl --data '{ "name": "F05" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/teams

We can then inspect the teams of the club:

    curl --request GET localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/teams

We can also fetch an individual team directly:

    curl --request GET localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/teams/1aa45de7-6ff0-4414-adfe-a645be7d4f98

We can also update a team:

    curl --data '{ "name": "P06" }' --header "Content-Type: application/json" --request PUT localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/teams/4bc5bcbc-b837-40c4-8f47-3a855b11ce8a

Or delete a team:

    curl --request DELETE localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/teams/1aa45de7-6ff0-4414-adfe-a645be7d4f98

Analogously, we can work with members related to the club (not that the <code>teamUuid</code> field is optional):

    curl --data '{ "name": "Henrik" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/members
    curl --data '{ "name": "Jon", "teamUuid": "4bc5bcbc-b837-40c4-8f47-3a855b11ce8a" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/members

Training phases can also be added:

    curl --data '{ "name": "Slå Henrik i armhävningar" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs/5f7e5a88-68bc-4581-8103-c8b2effc373d/training-phases

Finally, in order to allow for effective synchronization of club-related information, a special composite endpoint is offered:

    curl --request GET localhost:3000/0.0.0/club-composite/5f7e5a88-68bc-4581-8103-c8b2effc373d

The output of this will look something like this:

    {
        "members": [
            {
                "uuid":"119998b8-d8b1-45e8-9429-47f36d7f3de9",
                "name":"Henrik",
                "clubUuid":"5f7e5a88-68bc-4581-8103-c8b2effc373d"
            },
            {
                "uuid":"78a18f13-d17c-4541-af39-d1e7a0dd3c0b",
                "name":"Jon",
                "clubUuid":"5f7e5a88-68bc-4581-8103-c8b2effc373d",
                "teamUuid":"4bc5bcbc-b837-40c4-8f47-3a855b11ce8a"
            }
        ],
        "teams": [
            {
                "uuid":"4bc5bcbc-b837-40c4-8f47-3a855b11ce8a",
                "name":"P06",
                "clubUuid":"5f7e5a88-68bc-4581-8103-c8b2effc373d"
            }
        ],
        "trainingPhases": [
            {
                "uuid":"927069fa-d5eb-4e77-aa2b-2768dd528fc3",
                "name":"Slå Henrik i armhävningar",
                "clubUuid":"5f7e5a88-68bc-4581-8103-c8b2effc373d"
            }
        ],
        "name":"Bergsjö IF"
    }
