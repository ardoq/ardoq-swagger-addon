#!/bin/bash
confirm () {
    read -r -p "${1:-Are you sure? [y/N]} " response
    case $response in
        [yY][eE][sS]|[yY])
            true
            ;;
        *)
            false
            ;;
    esac
}

FROM_IMAGE=$(grep FROM Dockerfile|cut -d' ' -f2)

VERSION=$(grep "ENV VERSION" Dockerfile|cut -d' ' -f3)

TAGV="ardoq/ardoq-docker-compose-addon:$VERSION"
TAGL="ardoq/ardoq-docker-compose-addon:latest"

echo "building $TAG"
docker build -t $TAGL .
docker tag $TAGL $TAGV
confirm "Push $TAGV to DockerHub? [y/N]" && docker push $TAGV && docker push $TAGL



