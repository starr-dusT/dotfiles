#!/usr/bin/env bash
# https://github.com/starr-dusT/dotfiles

image_name="<image name>"
container_name="<container name>"

# if container image doesn't exist build it
if [ "$(docker images -q $image_name)" ]; then
    echo "Image exists don't need to build..."
else
    docker run -t "$image_name" .
fi

# if container doesn't exist run it else start/attach
if [ "$(docker ps -a --filter "status=exited" --format "{{.Names}}" -f name=$container_name)" ]; then
    echo "Attaching to existing container..."
    docker start "$container_name"
    docker attach "$container_name"
else
    echo "Running new container..."
    docker run --name "$container_name" --network host -v ./:/home/dev/src -it "$image_name"
fi
