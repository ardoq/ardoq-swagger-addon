#!/bin/sh
REPOSITORY="ardoq/ardoq-swagger-addon"

if [ "${CIRCLE_BRANCH}" == "master" ]
then
    DOCKER_TAG=$(head -n 1 project.clj | awk '{gsub(/"/, "", $3); print $3}')
elif  [ "${CIRCLE_BRANCH}" == "test" ] || [ "${CIRCLE_BRANCH:0:12}" = "internal-env" ]
then
    DOCKER_TAG=$(head -n 1 project.clj | awk '{gsub(/"/, "", $3); print $3}')-${CIRCLE_SHA1:0:7}
fi

RESPONSE=$(curl --write-out %{http_code} --silent --output /dev/null -H "Authorization: Basic $DOCKER_AUTH" \
                "https://hub.docker.com/v1/repositories/$REPOSITORY/tags/$DOCKER_TAG")

if [ $RESPONSE -eq 404 ]; then
    echo "Building docker container..."

    docker build -t $REPOSITORY:$DOCKER_TAG .
    docker tag $REPOSITORY:$DOCKER_TAG

    echo "Pushing $REPOSITORY:$DOCKER_TAG to private registry..."
    echo "{\"https://index.docker.io/v1/\":{\"auth\":\"$DOCKER_AUTH\",\"email\":\"$DOCKER_EMAIL\"}}" >> ~/.dockercfg
    docker push $REPOSITORY:$DOCKER_TAG

    if [ "${CIRCLE_BRANCH}" == "master" ]
    then
      docker tag $REPOSITORY:$DOCKER_TAG $REPOSITORY:latest
      docker push $REPOSITORY:latest
    elif [ "${CIRCLE_BRANCH}" == "test" ]
    then
      docker tag $REPOSITORY:$DOCKER_TAG $REPOSITORY:test
      docker push $REPOSITORY:test
    fi
    exit 0
elif [ $RESPONSE -eq 200 ]; then
    echo "Can't deploy version $REPOSITORY:$DOCKER_TAG, version already exists!"
    exit 1
else
    echo "Unhandled response code $RESPONSE, have you defined DOCKER_AUTH env variable?"
    exit 1
fi
