FROM nginx

RUN DEBIAN_FRONTEND=noninteractive apt-get update && apt-get -y install netcat

COPY nginx.conf /etc/nginx/nginx.conf
COPY snakeoil.key /etc/ssl/snakeoil.key
COPY snakeoil.crt /etc/ssl/snakeoil.crt
RUN chmod 400 /etc/ssl/snakeoil.*
COPY run.sh /app/run.sh
WORKDIR /app
CMD ["sh", "run.sh"]
