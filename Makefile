image:
	docker build -t hshort:$(git rev-parse --short HEAD) -f builder.dockerfile .
	docker cp hshort:$(git rev-parse --short HEAD)/app/hshort .
	docker build -f image.dockerfile .
