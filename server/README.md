# Server component for coachassistant

## Building

Configure the submodule dependency:

    $ git submodule init && git submodule update

Compiling (requires "libpq-dev" on Debian and "postgresql-devel" on Fedora):

    $ cabal sandbox delete
    $ cabal sandbox init
    $ cabal update
    $ cabal sandbox add-source ./lambdatrade-common
    $ cabal install

## API example using Curl

Let's start by adding a club:

    curl --data '{ "name": "Bergsjö IF" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs

Note that the service generates a UUID identifier for the club in the response:

    {"uuid":"5f7e5a88-68bc-4581-8103-c8b2effc373d","created":"2015-09-03T15:28:01.016843000000Z","teamUuids":[],"memberUuids":[],"trainingPhaseUuids":[],"videoUuids":[],"name":"Bergsjö IF"}

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

    curl --request GET localhost:3000/0.0.0/5f7e5a88-68bc-4581-8103-c8b2effc373d/composite

The output of this will look something like this:

    {
        "videos":
        [
        ],
        "members":
        [
            {
                "uuid": "92083708-5c47-4c2e-920d-193b5ffd8862",
                "created": "2015-09-02T16:09:48.423585000000Z",
                "videoUuids":
                [
                ],
                "name": "Henrik",
                "clubUuid": "4bdafb08-d8a5-40e2-83c6-f78f206f9e0f",
                "teamUuid": "e02897a1-adb2-4e61-bcd3-2846f6eeb7fe"
            },
            {
                "uuid": "80226dee-0459-4e97-a156-e3fb09c669e2",
                "created": "2015-09-02T16:09:43.959423000000Z",
                "videoUuids":
                [
                ],
                "name": "Jon",
                "clubUuid": "4bdafb08-d8a5-40e2-83c6-f78f206f9e0f",
                "teamUuid": "70d1d759-2238-4d21-95c5-6c63d7d9ab22"
            }
        ],
        "teams":
        [
            {
                "uuid": "70d1d759-2238-4d21-95c5-6c63d7d9ab22",
                "created": "2015-09-02T16:09:35.633229000000Z",
                "memberUuids":
                [
                    "80226dee-0459-4e97-a156-e3fb09c669e2"
                ],
                "videoUuids":
                [
                ],
                "name": "F10",
                "clubUuid": "4bdafb08-d8a5-40e2-83c6-f78f206f9e0f"
            },
            {
                "uuid": "e02897a1-adb2-4e61-bcd3-2846f6eeb7fe",
                "created": "2015-09-02T16:09:34.627710000000Z",
                "memberUuids":
                [
                    "92083708-5c47-4c2e-920d-193b5ffd8862"
                ],
                "videoUuids":
                [
                ],
                "name": "P10",
                "clubUuid": "4bdafb08-d8a5-40e2-83c6-f78f206f9e0f"
            }
        ],
        "trainingPhases":
        [
            {
                "uuid": "bd59ad1d-6c64-4d2b-a5f2-108c8ec5b931",
                "created": "2015-09-02T16:09:54.335738000000Z",
                "videoUuids":
                [
                ],
                "name": "Armhävningar",
                "clubUuid": "4bdafb08-d8a5-40e2-83c6-f78f206f9e0f"
            },
            {
                "uuid": "63603cc4-d0be-4cca-b5a9-8099957660ce",
                "created": "2015-09-02T16:09:56.506154000000Z",
                "videoUuids":
                [
                ],
                "name": "Uppvärmning",
                "clubUuid": "4bdafb08-d8a5-40e2-83c6-f78f206f9e0f"
            }
        ],
        "name": "Bergsjö IF",
	"uuid":"5f7e5a88-68bc-4581-8103-c8b2effc373d",
	"created":"2015-09-03T15:28:01.016843000000Z"
    }