#!/bin/sh
REPOSITORY="ardoq/ardoq-swagger-addon"
DOCKER_TAG=$(head -n 1 project.clj | awk '{gsub(/"/, "", $3); print $3}')

RESPONSE=$(curl --write-out %{http_code} --silent --output /dev/null -H "Authorization: Basic $DOCKER_AUTH" \
                "https://index.docker.io/v1/repositories/$REPOSITORY/tags/$DOCKER_TAG")

if [ $RESPONSE -eq 404 ]; then
    echo "Building docker container..."
    docker build -t $REPOSITORY:$DOCKER_TAG .
    docker tag $REPOSITORY:$DOCKER_TAG $REPOSITORY:latest
    echo "Pushing $REPOSITORY:$DOCKER_TAG to private registry..."
    echo "{\"https://index.docker.io/v1/\":{\"auth\":\"$DOCKER_AUTH\",\"email\":\"$DOCKER_EMAIL\"}}" >> ~/.dockercfg
    docker push $REPOSITORY:$DOCKER_TAG
    docker push $REPOSITORY:latest
    exit 0
elif [ $RESPONSE -eq 200 ]; then
    echo "Can't deploy version $REPOSITORY:$DOCKER_TAG, version already exists!"
    exit 1
else
    echo "Unhandled response code $RESPONSE, have you defined DOCKER_AUTH env variable?"
    exit 1
fi
