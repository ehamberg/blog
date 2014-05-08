build:
	cabal exec runhaskell blog.hs build

preview:
	cabal exec runhaskell blog.hs preview

deploy:
	cabal exec runhaskell blog.hs deploy

clean:
	cabal exec runhaskell blog.hs clean
