# For building the Compiler and the Runtime please look at Makefiles in
# their directories

.PHONY: docs
docs:
	cd docs-src && \
	$(MAKE) html && \
	rm -rf ../docs/* && \
	mv -f _build/html/* _build/html/.n* ../docs/