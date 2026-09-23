.PHONY: docker docker-lambda run

RES_VERSION := $(shell ./get_version.sh)

docker:
	docker build --tag tamil-dictionary --build-arg RES_VERSION=${RES_VERSION} .

docker-lambda: docker
	docker build --tag tamil-dictionary-lambda -f Dockerfile.lambda .

run: docker
	docker run --publish 8000:8000 tamil-dictionary
