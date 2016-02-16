STACK=stack --resolver=lts-5.2 runghc --package hakyll
build:
	$(STACK) blog.hs build

preview:
	$(STACK) blog.hs preview

deploy:
	$(STACK) blog.hs deploy

clean:
	$(STACK) blog.hs clean
