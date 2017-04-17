# healthcheck
A dummy project implementing a health check for testing purposes.

## Get healthcheck state
curl http://127.0.0.1:8080/healthcheck

## Set healthcheck state
curl -X PUT -d "true" http://127.0.0.1/healthcheck
