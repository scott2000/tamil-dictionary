.PHONY: docker docker-lambda run

RESOURCE_PATH := /res

docker:
	docker build --tag tamil-dictionary --build-arg RESOURCE_PATH=${RESOURCE_PATH} .

docker-lambda: docker
	docker build \
		-f Dockerfile.lambda \
		--tag tamil-dictionary-lambda \
		--platform linux/amd64 \
		--provenance false \
		.

run: docker
	docker run --publish 8000:8000 tamil-dictionary
