
.PHONY: build run test clean

build:
	@echo "Building..."
	@cabal build
	@cp dist/build/lambda-blog.fcgi/lambda-blog.fcgi data/
	@mkdir -p tmp

run: build
	./scripts/server.sh

configure:
	cabal configure

test:
	export LAMBDA_BLOG_DIR="./test/data" && runghc -isrc:test/src test/src/runtests.hs

clean:
	rm -rf data/lambda-blog.fcgi data/lambda.log tmp dist

