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

    $ docker build -t tranarappen-server server
    $ docker build -t tranarappen-web web
    $ docker save --output tranarappen-server.tar tranarappen-server:latest
    $ docker save --output tranarappen-web.tar tranarappen-web:latest

Do this remotely:

    $ docker load < tranarappen-server.tar
    $ docker load < tranarappen-web.tar
    $ docker stop tranarappen-web-container
    $ docker stop tranarappen-server-container
    $ docker rm tranarappen-web-container
    $ docker rm tranarappen-server-container
    $ docker create --name=server --link=database -P tranarappen-server
    $ docker create --name=web --link=server -p 80:80 -p 443:443 -p 3000:3000 -v ~/ssl/app.tranarappen.se/server.crt:/etc/ssl/tranarappen.crt -v ~/ssl/app.tranarappen.se/server.key:/etc/ssl/tranarappen.key tranarappen-web
    $ docker start server
    $ docker start web

The grass picture is in the public domain and is taken from
https://pixabay.com/en/grass-field-football-lawn-green-966410/.