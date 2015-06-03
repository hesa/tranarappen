# Server component for coachassistant

## Building and running

Docker and Fig needs to be installed.

Run the project:

    $ git submodule init && git submodule update
    $ fig up

For rebuilding the project, do the following:

    $ fig kill && fig rm --force && fig build && fig up

## API example using Curl

Let's start by adding a club:

    curl --data '{ "name": "Bergsjö IF" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs

Note that the service generates a UUID identifier for the club in the response:

    {"uuid":"31ffe0aa-149d-4696-9620-3a48898e1fa1","name":"Bergsjö IF"}

We can use this identifier to, for instance, access the teams of the club, like so:

    curl --request GET localhost:3000/0.0.0/clubs/31ffe0aa-149d-4696-9620-3a48898e1fa1/teams

We can also, for instance, create teams for the club:

    curl --data '{ "name": "P05" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs/31ffe0aa-149d-4696-9620-3a48898e1fa1/teams
    curl --data '{ "name": "F05" }' --header "Content-Type: application/json" --request POST localhost:3000/0.0.0/clubs/31ffe0aa-149d-4696-9620-3a48898e1fa1/teams

We can then inspect the teams of the club:

    curl --request GET localhost:3000/0.0.0/clubs/31ffe0aa-149d-4696-9620-3a48898e1fa1/teams

We can also fetch an individual team directly:

    curl --request GET localhost:3000/0.0.0/clubs/31ffe0aa-149d-4696-9620-3a48898e1fa1/teams/4b9367e5-3479-4ed8-9555-18308100319a

We can also update a team:

    curl --data '{ "name": "F06" }' --header "Content-Type: application/json" --request PUT localhost:3000/0.0.0/clubs/31ffe0aa-149d-4696-9620-3a48898e1fa1/teams/28286a61-6eeb-40a4-825b-eca85bf98e0a

Or delete a team:

    curl --request DELETE localhost:3000/0.0.0/clubs/31ffe0aa-149d-4696-9620-3a48898e1fa1/teams/4b9367e5-3479-4ed8-9555-18308100319a
