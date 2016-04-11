#!/bin/sh

until nc -z authweb 80; do
    echo "Waiting for auth_web..."
    sleep 1
done

echo "Web server is running!"

cd /etc/nginx && nginx -g "daemon off;"
