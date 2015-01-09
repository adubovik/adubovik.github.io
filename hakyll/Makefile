all: build

build: hakyll
	./hakyll build

rebuild: hakyll
	./hakyll rebuild

watch: build
	./hakyll watch

deploy: rebuild
	./hakyll deploy

hakyll: Main.hs
	ghc Main.hs -o hakyll

clean:
	./hakyll clean
	rm hakyll
	rm -v Main{.o,.hi}
