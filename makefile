CMAKE       := cmake .. -GNinja -DCMAKE_BUILD_TYPE=Debug
MKDIR_BUILD := mkdir -p build && cd build

git: clean
	git add . && \
	git commit -m "$(shell hostname)" && \
	git push

.PHONY: test
test:
	$(MKDIR_BUILD) && $(CMAKE) && ninja && ctest -VV

clean:
	rm -rf build
	rm -rf bak
	rm -f tags*
