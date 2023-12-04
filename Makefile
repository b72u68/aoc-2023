TEMPLATEFILE="lib/template.ml"
DAY=$(shell TZ=America/New_York date '+%d')
FILENAME="day${DAY}"
DIRNAME="lib/${FILENAME}.ml"

.PHONY: all

all:
	@if [ -f ${DIRNAME} ]; then \
		echo "${DIRNAME} has already existed."; \
	else \
		touch ${DIRNAME}; \
		cp ${TEMPLATEFILE} ${DIRNAME}; \
		touch "input/${FILENAME}.txt"; \
		echo "${DIRNAME} created successfully."; \
	fi

build: lib/ bin/
	dune build --profile release

run:
	dune exec aoc_2023 --profile release
