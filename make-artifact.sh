#! /bin/sh
#
# Prepare artifact zip archive
#
# It includes
# - artifact.pdf: This file
# - pnut-image.tar.gz: A docker image containing pnut and its dependencies
# - pnut/: pnut's source code
# - pnut.sh/: Accompanying website's source code

set -e -u

ZIP_DIR=artifact
ZIP_FILE=artifact.zip
DOCKER_IMAGE_TAG=pnut-artifact

# Create zip directory
rm -rf "$ZIP_DIR"
mkdir -p "$ZIP_DIR"

pandoc artifact.md -o "$ZIP_DIR/artifact.pdf"

# Build docker image
docker build . \
  -t "$DOCKER_IMAGE_TAG" \
  --build-arg PNUT_SOURCE=clone \
  --platform linux/amd64 \
  --no-cache

# Export it
docker save "$DOCKER_IMAGE_TAG" | gzip > "$ZIP_DIR/$DOCKER_IMAGE_TAG-image.tar.gz"

# Copy pnut source code
git clone https://github.com/udem-dlteam/pnut.git --depth 1 "./$ZIP_DIR/pnut"

# Copy website source code
git clone https://github.com/udem-dlteam/pnut.sh --depth 1 "./$ZIP_DIR/pnut-website"

# Create zip archive
zip -r -q "$ZIP_FILE" "$ZIP_DIR"

echo "Artifact zip archive created: $ZIP_FILE"
