#!/bin/sh

STACK=swagger
VERSION=`grep "ENV VERSION" Dockerfile| awk '{gsub(/"/, "", $3); print $3}'`

if [ "${CIRCLE_BRANCH}" == "master" ]; then
    aws sqs send-message --queue-url https://sqs.eu-west-1.amazonaws.com/468209928328/ardoq-production-swarm-queue  --message-body "{\"stack\":\"${STACK}\",\"version\":\"${VERSION}\"}" --region eu-west-1
fi

if [ "${CIRCLE_BRANCH}" == "test" ]; then
    aws sqs send-message --queue-url https://sqs.eu-central-1.amazonaws.com/468209928328/ardoq-test-swarm-queue --message-body "{\"stack\":\"${STACK}\",\"version\":\"${VERSION}\"}" --region eu-central-1
fi
