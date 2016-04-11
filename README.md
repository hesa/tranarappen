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
3. ~/volumes/app.conf (web/app)
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


The grass picture is in the public domain and is taken from
https://pixabay.com/en/grass-field-football-lawn-green-966410/.