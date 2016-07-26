FROM haskell:7.8

EXPOSE 80

RUN echo "deb http://ftp.debian.org/debian jessie-backports main contrib non-free" >> /etc/apt/sources.list
RUN apt-get update
RUN apt-get -y upgrade
RUN apt-get -y install ffmpeg libpq-dev netcat
RUN cabal update

COPY ./nejla-common /opt/dependencies/nejla-common
COPY coachassistant.cabal /opt/dependencies/coachassistant.cabal
COPY run.sh /app/run.sh

RUN cd /opt/dependencies && cabal --ignore-sandbox install --only-dep . ./nejla-common -j4 --force-reinstall

COPY . /opt/app

RUN cd /opt/app && cabal --ignore-sandbox install . ./nejla-common --force-reinstall

ENV PATH /root/.cabal/bin:$PATH

WORKDIR /app
RUN mkdir /app/upload
RUN mkdir /app/videos
CMD ["sh", "run.sh"]
