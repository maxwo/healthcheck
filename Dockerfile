FROM openjdk:8-jre-alpine

MAINTAINER Maxime Wojtczak <maxime.wojtczak@zenika.com>

ENV PROJECT healthcheck
ENV VERSION 0.1.0

COPY target/$PROJECT-$VERSION.jar /healthcheck.jar

EXPOSE 8080

ENTRYPOINT ["java"]
CMD ["-jar", "/healthcheck.jar"]