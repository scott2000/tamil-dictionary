.PHONY: docker docker-lambda run

RES_VERSION := $(shell ./get_version.sh)
LAMBDA_RES_BASE_PATH := /res/

docker:
	docker build --tag tamil-dictionary --build-arg RES_VERSION=${RES_VERSION} .

docker-lambda: docker
	docker build \
		-f Dockerfile.lambda \
		--tag tamil-dictionary-lambda \
		--platform linux/amd64 \
		--provenance false \
		--build-arg RES_BASE_PATH=${LAMBDA_RES_BASE_PATH} \
		.

run: docker
	docker run --publish 8000:8000 tamil-dictionary
