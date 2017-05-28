# healthcheck
[![Build Status](https://travis-ci.org/maxwo/healthcheck.svg?branch=master)](https://travis-ci.org/maxwo/healthcheck) [![Coverage Status](https://coveralls.io/repos/github/maxwo/healthcheck/badge.svg?branch=master)](https://coveralls.io/github/maxwo/healthcheck?branch=master) [![Codacy Badge](https://api.codacy.com/project/badge/Grade/931f06b2a71446deb6bf58258ea83d8b)](https://www.codacy.com/app/maxwo/healthcheck?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=maxwo/healthcheck&amp;utm_campaign=Badge_Grade)

A dummy project implementing a health check for testing purposes.

## Get healthcheck state
curl http://127.0.0.1:8080/healthcheck

## Set healthcheck state
curl -X PUT -H 'Content-type: application/json' -d 'true' http://127.0.0.1:8080/healthcheck

## Healthcheck endpoint
curl http://127.0.0.1:8080/health

It should return a 200 OK status when healthy, otherwise, 5XX.
