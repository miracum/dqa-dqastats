#!/bin/bash
set -e 
set -o pipefail

## Get version tag and registry-prefix from .env:
source ./.env

## Should the docker building process build without caching? (true/false)
docker_build_no_cache=false

printf "\n\n##################################\n"
printf "Building images with version tag $IMAGE_TAG"
printf "\n##################################\n"

printf "\n\nPlease insert your login credentials to registry: $REGISTRY_PREFIX ...\n"
docker login

## Base image:
printf "\n\n##################################\n"
printf "$REGISTRY_PREFIX/$IMAGE_NAME:$IMAGE_TAG"
printf "\n##################################\n"
printf "\nPulling cached $IMAGE_NAME image\n"
# pull latest image for caching:
docker pull $REGISTRY_PREFIX/$IMAGE_NAME
# build new image (latest):
printf "\n\nBuilding $IMAGE_NAME image\n"
cd ..
docker build \
    --progress=plain \
    --no-cache=${docker_build_no_cache} \
    --build-arg CACHEBREAKER=blabla \
    -f docker/Dockerfile \
    -t $REGISTRY_PREFIX/$IMAGE_NAME \
    . 2>&1 | tee ./log_$IMAGE_NAME.log
cd docker
printf "\n\nPushing $IMAGE_NAME image (latest)\n"
# push new image as new 'latest':
docker push "$REGISTRY_PREFIX/$IMAGE_NAME"
# also tag it with the new tag:
docker tag $REGISTRY_PREFIX/$IMAGE_NAME $REGISTRY_PREFIX/$IMAGE_NAME:$IMAGE_TAG
# and also push this (tagged) image:
printf "\n\nPushing $IMAGE_NAME image ($IMAGE_TAG)\n"
docker push "$REGISTRY_PREFIX/$IMAGE_NAME:$IMAGE_TAG"

# ## Push image to second registry:
# docker tag $REGISTRY_PREFIX/$IMAGE_NAME:$IMAGE_TAG $REGISTRY_PREFIX2/$IMAGE_NAME:$IMAGE_TAG
# printf "\n\nPlease insert your login credentials to registry: $REGISTRY_PREFIX2 ...\n"
# docker login "https://$REGISTRY_PREFIX2"
# printf "\n## Pushing image $REGISTRY_PREFIX2/$IMAGE_NAME:$IMAGE_TAG...\n"
# docker push $REGISTRY_PREFIX2/$IMAGE_NAME:$IMAGE_TAG
# ## Also tag and push the latest image:
# printf "\n## Pushing image $REGISTRY_PREFIX2/$IMAGE_NAME...\n"
# docker tag $REGISTRY_PREFIX2/$IMAGE_NAME:$IMAGE_TAG $REGISTRY_PREFIX2/$IMAGE_NAME
# docker push $REGISTRY_PREFIX2/$IMAGE_NAME
