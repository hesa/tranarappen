# coachassistant

A software suite to assist coaches/instructors during actual training.

## Building and running

Docker needs to be installed.

First, build the server and web projects by following the instructions in the
respective README files in the "server" and "web" directories.

On Fedora 22, it might be necessary to configure/disable SELinux and do the
following:

    $ sudo firewall-cmd --permanent --zone=public --add-interface=docker0
    $ sudo firewall-cmd --permanent --zone=public --add-masquerade
    $ systemctl restart firewalld
    $ systemctl restart docker

Configure the submodules:

    $ git submodule init && git submodule update

Build the web application:

    $ sudo apt-get install --yes nodejs nodejs-legacy npm
    $ sudo npm -g install bower grunt-cli jslint
    $ cd web
    $ npm install
    $ bower install
    $ grunt deps
    $ grunt dev
    $ ^C
    $ cd ..

<code>grunt dev</code> will listen to any changes to the JavaScript, HTML or CSS
files in the source directory and will, upn changes, trigger a "rebuild" of the
source code. <code>grunt deps</code> will fetch and rebuild the Bower
dependencies. (You can also use <code>grunt live</code> to, in addition to
performing the tasks of the two previous tasks, run JSLint on the source code.)

Run the project:

    $ docker-compose up

The server will now be running at https://localhost/. We're using a snakeoil
HTTPS certificate (supposed to be overwritten in production) that you will
likely have to add as an exception to your browser's HTTPS settings.

Add a user to the authentication layer like this:

    $ docker exec -it coachassistant_authservice_1 auth-service adduser email@example.com password "Full Name"

Create the "instance" and give the user access to it:

    $ docker exec -it coachassistant_database_1 psql --user postgres
    $ insert into instance values ('c1a1501b-f0af-4c2e-b925-a2aad7b61335', 'server');
    $ insert into user_instance ("user", instance_id) select uuid, 'c1a1501b-f0af-4c2e-b925-a2aad7b61335' from "user";

For rebuilding the project, do the following:

    $ cd server
    $ git submodule update
    $ cd ..
    $ docker-compose kill && \
      docker-compose rm --force && \
      docker-compose build && \
      docker-compose up

## Building for deployment

One-time thing to do remotely:

    $ docker create --name=database postgres:9.4
    $ docker start database

Do this locally:

    $ docker build -t auth-service auth-service
    $ docker build -t auth-web auth-service/web
    $ docker build -t tranarappen-server server
    $ docker save --output auth-service.tar auth-service:latest
    $ docker save --output auth-web.tar auth-web:latest
    $ docker save --output tranarappen-server.tar tranarappen-server:latest

Upload the following files to the server (and make sure that ~/ssl/app.tranarappen.se/server.key and ~/ssl/app.tranarappen.se/server.crt exists on the server):

1. ~/volumes/nginx-web.conf (web/nginx.conf)
2. ~/volumes/nginx-router.conf (router/nginx.conf)
3. ~/volumes/app (web/app)
4. ~/volumes/auth-service.include (auth-service/web/auth-service.include)

... and do this remotely:

    $ docker load < auth-service.tar
    $ docker load < auth-web.tar
    $ docker load < tranarappen-server.tar
    $ docker stop server
    $ docker stop auth-web-container
    $ docker stop auth-service-container
    $ docker rm server
    $ docker rm auth-service-container
    $ docker rm auth-web-container
    $ docker create --name=server --link=database tranarappen-server
    $ docker create --name=auth-service-container --link=database auth-service
    $ docker create --name=auth-web-container --link=auth-service-container:authservice --link=server:c1a1501b-f0af-4c2e-b925-a2aad7b61335 auth-web
    $ docker create --name=web --link=auth-web-container:authweb -v ~/volumes/app:/www -v ~/volumes/nginx-web.conf:/etc/nginx/nginx.conf -v ~/volumes/auth-service.include:/etc/nginx/auth-service.include nginx
    $ docker create --name=router --link=web -v ~/volumes/nginx-router.conf:/etc/nginx/nginx.conf -v ~/ssl/app.tranarappen.se/server.crt:/etc/ssl/tranarappen.crt -v ~/ssl/app.tranarappen.se/server.key:/etc/ssl/tranarappen.key -p 443:443 nginx
    $ docker start server
    $ docker start auth-service-container
    $ docker start auth-web-container
    $ docker start web
    $ docker start router

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

## Legal

The grass picture is in the public domain and is taken from
https://pixabay.com/en/grass-field-football-lawn-green-966410/.
