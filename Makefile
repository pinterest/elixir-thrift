# Makefile targets for setting up the Travis-CI build and test environment

.PHONY: install-thrift

THRIFT_VERSION ?= 0.9.3

all:

$(HOME)/vendor:
	mkdir -p $(HOME)/vendor

$(HOME)/vendor/thrift-$(THRIFT_VERSION).tar.gz: $(HOME)/vendor
	cd $(HOME)/vendor && curl -O http://www.apache.org/dist/thrift/$(THRIFT_VERSION)/thrift-$(THRIFT_VERSION).tar.gz

$(HOME)/vendor/thrift-$(THRIFT_VERSION): $(HOME)/vendor/thrift-$(THRIFT_VERSION).tar.gz
	cd $(HOME)/vendor && tar xfz thrift-$(THRIFT_VERSION).tar.gz

install-thrift: $(HOME)/vendor/thrift-$(THRIFT_VERSION)
	cd $(HOME)/vendor/thrift-$(THRIFT_VERSION) && ./configure --prefix=$(HOME) --with-erlang --without-csharp --without-java --without-nodejs --without-lua --without-python --without-perl --without-php --without-php_extension --without-ruby --without-haskell --without-go --without-haxe --without-d && make install
