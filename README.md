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

Add the auth-service to the repository somehow, perhaps like this:

    $ git clone git@github.com:nejla/auth-service.git

Run the project:

    $ docker-compose up

The server will now be running at https://localhost/. We're using a snakeoil
HTTPS certificate that you will likely have to add as an exception to your
browser's HTTPS settings.

Add a user to the authentication layer like this:

    $ docker exec -it coachassistant_auth-service_1 auth-service adduser email@example.com password "Full Name"

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

The grass picture is in the public domain and is taken from
https://pixabay.com/en/grass-field-football-lawn-green-966410/.