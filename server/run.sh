#!/bin/sh

until nc -z database 5432; do
    echo "Waiting for PostgreSQL..."
    sleep 1
done

echo "Giving PostgreSQL some time to start up..."
sleep 5

echo "Server is running!"

exec coachassistant run
