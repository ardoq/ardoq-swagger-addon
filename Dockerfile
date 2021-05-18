FROM ardoq/leiningen:3.3-8u74-2.6.1
MAINTAINER Kristian Helgesen "kristian@ardoq.com"

ENV VERSION 1.0.18
ENV LEIN_ROOT disable
ENV API_BASE_URL https://app.ardoq.com

COPY ./target/ardoq-swagger-addon-$VERSION-standalone.jar /usr/src/app/app-standalone.jar

EXPOSE 80
WORKDIR /usr/src/app
CMD ["java", "-jar", "app-standalone.jar"]
