.PHONY: docker docker-lambda run

docker:
	docker build --tag tamil-dictionary \
		--build-arg BASE_PATH=${BASE_PATH} \
		--build-arg RESOURCE_PATH=${RESOURCE_PATH} \
		.

docker-lambda: docker
	docker build \
		-f Dockerfile.lambda \
		--tag tamil-dictionary-lambda \
		--platform linux/amd64 \
		--provenance false \
		--build-arg BASE_PATH=${BASE_PATH} \
		.

run: docker
	docker run --publish 8000:8000 tamil-dictionary
