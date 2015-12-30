# Test suite stub for the Coachassistant web service

First, make sure that Firefox accepts the snakeoil certificate that we are currently using (or switch the Selenium browser in conf.js).

Second, install the Protractor testing framework and get Selenium up and running.

    $ sudo npm install -g protractor
    $ sudo webdriver-manager update
    $ webdriver-manager start

If your version of NodeJS is too low (< 3.0, probably), you can install the latest version by doing something like the following:

    $ wget https://nodejs.org/dist/v4.2.4/node-v4.2.4-linux-x64.tar.gz
    $ sudo tar xvvf node-v4.2.4-linux-x64.tar.gz --strip-components=1 -C /usr/local node-v4.2.4-linux-x64
    $ nodejs --version

Now you should be able to run the test with:

    $ protractor conf.js