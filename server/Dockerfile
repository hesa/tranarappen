FROM haskell:7.8

RUN apt-get update
RUN apt-get -y upgrade
RUN apt-get -y install libpq-dev netcat
RUN apt-get -y install libav-tools
RUN cabal update

COPY ./lambdatrade-common /opt/dependencies/lambdatrade-common
COPY coachassistant-server.cabal /opt/dependencies/coachassistant-server.cabal
COPY run.sh /app/run.sh

RUN cd /opt/dependencies && cabal --ignore-sandbox install --only-dep . ./lambdatrade-common -j4 --force-reinstall

COPY . /opt/app

RUN cd /opt/app && cabal --ignore-sandbox install . ./lambdatrade-common --force-reinstall

ENV PATH /root/.cabal/bin:$PATH

WORKDIR /app
RUN mkdir /app/upload
RUN mkdir /app/videos
CMD ["sh", "run.sh"]
