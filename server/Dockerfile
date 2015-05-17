FROM haskell:7.8

RUN apt-get update
RUN apt-get -y upgrade
RUN apt-get -y install libpq-dev netcat
RUN cabal update

COPY ./lambdatrade-common /opt/cabal/lambdatrade-common
COPY coachassistant-server.cabal /opt/cabal/coachassistant-server.cabal
COPY run.sh /app/run.sh

RUN cd /opt/cabal && cabal --ignore-sandbox install --only-dep . ./lambdatrade-common -j4 --force-reinstall

COPY . /opt/app

RUN cd /opt/app && cabal --ignore-sandbox install . ./lambdatrade-common --force-reinstall

ENV PATH /root/.cabal/bin:$PATH

WORKDIR /app
CMD ["sh", "run.sh"]
