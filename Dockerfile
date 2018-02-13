FROM ardoq/leiningen:3.5-8u131-2.7.1 as builder
RUN mkdir -p /usr/src/app

ENV VERSION 1.3
ENV LEIN_ROOT disable

WORKDIR /usr/src/app

COPY project.clj /usr/src/app/
RUN lein deps

COPY . /usr/src/app
RUN mv "$(lein uberjar | sed -n 's/^Created \(.*standalone\.jar\)/\1/p')" app-standalone.jar


FROM ardoq/leiningen:3.5-8u131-2.7.1
COPY --from=builder /usr/src/app/app-standalone.jar /usr/src/app/app-standalone.jar

ENV VERSION 1.3
ENV LEIN_ROOT disable
ENV API_BASE_URL https://app.ardoq.com

EXPOSE 80
WORKDIR /usr/src/app
CMD ["java", "-jar", "app-standalone.jar"]
