#!/bin/sh

echo "Web server is running!"

cd /etc/nginx && nginx -g "daemon off;"
