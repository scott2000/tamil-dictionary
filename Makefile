.PHONY: docker docker-lambda

RES_VERSION := $(shell ./get_version.sh)

docker:
	docker build --tag tamil-dictionary --build-arg RES_VERSION=${RES_VERSION} .

docker-lambda: docker
	docker build --tag tamil-dictionary-lambda -f Dockerfile.lambda .
