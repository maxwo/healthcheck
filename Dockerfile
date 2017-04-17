FROM openjdk:8-jre-alpine

MAINTAINER Maxime Wojtczak <maxime.wojtczak@zenika.com>

ENV PROJECT healthcheck
ENV VERSION 0.1.0-SNAPSHOT

COPY target/$PROJECT-$VERSION.jar /$PROJECT.jar

EXPOSE 8080

ENTRYPOINT ["java"]
CMD ["-jar", "/$PROJECT.jar"]