# coachassistant

A software suite to assist instructors with coaching.

## Building and running

Docker needs to be installed.

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

To configure the marketing web site, Bootstrap should be downloaded and
installed in (the default) css, fonts and js directories, in ./marketing.

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
    $ docker stop router
    $ docker stop web
    $ docker stop server
    $ docker stop auth-web-container
    $ docker stop auth-service-container
    $ docker rm router
    $ docker rm web
    $ docker rm server
    $ docker rm auth-service-container
    $ docker rm auth-web-container
    $ docker create --name=server --link=database tranarappen-server
    $ docker create --name=auth-service-container --link=database auth-service
    $ docker create --name=auth-web-container --link=auth-service-container:authservice --link=server:c1a1501b-f0af-4c2e-b925-a2aad7b61335 auth-web
    $ docker create --name=web --link=auth-web-container:authweb -v ~/volumes/app:/www -v ~/volumes/nginx-web.conf:/etc/nginx/nginx.conf -v ~/volumes/auth-service.include:/etc/nginx/auth-service.include nginx
    $ docker create --name=router --link=web -v ~/volumes/nginx-router.conf:/etc/nginx/nginx.conf -v ~/ssl/app.tranarappen.se/server.crt:/etc/ssl/tranarappen.crt -v ~/ssl/app.tranarappen.se/server.key:/etc/ssl/tranarappen.key -p 443:443 nginx

Please note that the auth-web-container should have a "client_max_body_size" in the http stanza in /etc/nginx/nginx.conf that mathes the current configuration of the service (otherwise the Nginx default limit will apply, and that's not sufficient for video uploading).

    $ docker start server
    $ docker start auth-service-container
    $ docker start auth-web-container
    $ docker start web
    $ docker start router

## API example using Curl

Before you can start to use the API, you will need to authenticate and acquire a token and the instance identifier. This is done like [this](https://github.com/nejla/auth-service#api). The token should be sent using the <code>X-Token</code> header, and the instance identifier should be sent using the <code>X-Instance</code> header. The instance identifier correlate to a club.

To make the example requests below shorter, it's assumed that you are including the token and the instance identifier, like so: <code>--header "X-Instance: ..." --header "X-Token: ..."</code>.

First, let's add two teams.

    curl --data '{ "name": "P05" }' --header "Content-Type: application/json" --header "X-Instance: ..." --header "X-Token: ..." --request POST https://app.tranarappen.se/0.0.0/teams
    curl --data '{ "name": "F05" }' --header "Content-Type: application/json" --header "X-Instance: ..." --header "X-Token: ..." --request POST https://app.tranarappen.se/0.0.0/teams

We can then inspect the teams of the club:

    curl --request GET https://app.tranarappen.se/0.0.0/teams

We can also fetch an individual team directly:

    curl --request GET https://app.tranarappen.se/0.0.0/teams/1aa45de7-6ff0-4414-adfe-a645be7d4f98

We can also update a team:

    curl --data '{ "name": "P06" }' --header "Content-Type: application/json" --request PUT https://app.tranarappen.se/0.0.0/teams/4bc5bcbc-b837-40c4-8f47-3a855b11ce8a

Or delete a team:

    curl --request DELETE https://app.tranarappen.se/0.0.0/teams/1aa45de7-6ff0-4414-adfe-a645be7d4f98

Analogously, we can work with members related to the club (not that the <code>teamUuid</code> field is optional):

    curl --data '{ "name": "Henrik" }' --header "Content-Type: application/json" --request POST https://app.tranarappen.se/0.0.0/members
    curl --data '{ "name": "Jon", "teamUuid": "4bc5bcbc-b837-40c4-8f47-3a855b11ce8a" }' --header "Content-Type: application/json" --request POST https://app.tranarappen.se/0.0.0/members

Training phases can also be added:

    curl --data '{ "name": "Slå Henrik i armhävningar" }' --header "Content-Type: application/json" --request POST https://app.tranarappen.se/0.0.0/training-phases

Finally, in order to allow for effective synchronization of club-related information, a special composite endpoint is offered:

    curl --request GET https://app.tranarappen.se/0.0.0/5f7e5a88-68bc-4581-8103-c8b2effc373d/composite

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
                "teamUuid": "e02897a1-adb2-4e61-bcd3-2846f6eeb7fe"
            },
            {
                "uuid": "80226dee-0459-4e97-a156-e3fb09c669e2",
                "created": "2015-09-02T16:09:43.959423000000Z",
                "videoUuids":
                [
                ],
                "name": "Jon",
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
                "name": "F10"
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
                "name": "P10"
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
                "name": "Armhävningar"
            },
            {
                "uuid": "63603cc4-d0be-4cca-b5a9-8099957660ce",
                "created": "2015-09-02T16:09:56.506154000000Z",
                "videoUuids":
                [
                ],
                "name": "Uppvärmning"
            }
        ]
    }

Uploading a video starts with defining the metadata of the video. The "recorded" field needs to contain the time when the video was recorded, in the format of an "ISO string". The training phase also need to be provided. The member identifier ("memberUuid") is optional - if it's absent, the system considers the video to be an instructional video.

    $ curl --data "{ \"recorded\": \"2015-12-29T19:31:12.081528Z\", \"trainingPhaseUuid\": \"bd59ad1d-6c64-4d2b-a5f2-108c8ec5b931\" }" --header "Content-Type: application/json" --request POST https://app.tranarappen.se/0.0.0/videos
    {"status":"empty","uuid":"eb0a50fe-a585-4d88-a37a-268a01d7a528","created":"2015-12-30T02:47:38.058862Z","recorded":"2015-12-29T19:31:12.081528Z","trainingPhaseUuid":"bd59ad1d-6c64-4d2b-a5f2-108c8ec5b931"}

For brevity, we'll save the video UUID in an environment variable.

    $ export VIDEO_UUID=eb0a50fe-a585-4d88-a37a-268a01d7a528

At this point, the video exists, but since it doesn't have any video data associated with it, it won't be visible on the site (it's "empty"). However, since we know the video identifier, we can query the video metadata like so:

    $ curl --header "Content-Type: application/json" https://app.tranarappen.se/0.0.0/videos/uuid/$VIDEO_UUID
    {"status":"empty","uuid":"eb0a50fe-a585-4d88-a37a-268a01d7a528","created":"2015-12-30T02:47:38.058862Z","trainingPhaseUuid":"bd59ad1d-6c64-4d2b-a5f2-108c8ec5b931"}

Now, let's upload some video data:

    $ curl --data-binary @sample.3gp --insecure --request POST https://app.tranarappen.se/api/0.0.0/videos/uuid/$VIDEO_UUID/upload

The API will now begin processing the video data into our desired format (WebM). If we repeat the GET request performed above, we can see that the status of the video now is "processing". This can be the basis to, for instance, show a loading animation in the client.

    $ curl --header "Content-Type: application/json" https://app.tranarappen.se/0.0.0/videos/uuid/$VIDEO_UUID
    {"status":"processing","uuid":"eb0a50fe-a585-4d88-a37a-268a01d7a528","created":"2015-12-30T02:47:38.058862Z","trainingPhaseUuid":"bd59ad1d-6c64-4d2b-a5f2-108c8ec5b931"}

The video file, around 30 MB, took between two and three minutes to process on my machine. However, since this happens in a separate thread, the API was still responsive during that time.

Once the video has finished processing, the status will changed to "complete" (unless Ffmpeg finishes with a non-zero exit code, in which case it will change to "failure"):

    $ curl --header "Content-Type: application/json" https://app.tranarappen.se/0.0.0/videos/uuid/$VIDEO_UUID
    {"status":"complete","uuid":"eb0a50fe-a585-4d88-a37a-268a01d7a528","created":"2015-12-30T02:47:38.058862Z","published":"2015-12-30T02:51:03.262879Z","trainingPhaseUuid":"bd59ad1d-6c64-4d2b-a5f2-108c8ec5b931"}

We can now download the video with a simple GET request.

    $ curl --insecure -o sample.webm https://app.tranarappen.se/api/0.0.0/videos/uuid/$VIDEO_UUID/download

It's also possible to download a poster of the video:

    $ curl --insecure -o sample.jpeg https://app.tranarappen.se/api/0.0.0/videos/uuid/$VIDEO_UUID/poster

Training phase objects will include the video identifier for the training phase in question.

Also, videos can be filtered in one of the following ways:

* By member
* By member and training phase
* By team
* By team and training phase
* By training phase (not showing instructional videos)
* By whether the video is instructional

## Legal

The grass picture is in the public domain and is taken from
https://pixabay.com/en/grass-field-football-lawn-green-966410/.

The soccer screenshot in marketing/screenshots.png is in the public domain and
is taken from <https://pixabay.com/en/football-ball-sport-soccer-play-452569/>.
