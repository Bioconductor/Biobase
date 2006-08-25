TOP=../../..
RUNIT_DIR=$(shell pwd)

all: install test

install:
	cd ${TOP}; \
	R CMD INSTALL --clean Biobase && \
	cd ${RUNIT_DIR}

test:
	R --slave < runalltests.R

%.R: FORCE
	./runfile.sh $@

FORCE: