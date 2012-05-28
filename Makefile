build:
	runhaskell blog.hs build

preview: build
	runhaskell blog.hs preview

deploy: build
	runhaskell blog.hs deploy

clean:
	rm -rf _site/*
	rm -rf _cache/*
