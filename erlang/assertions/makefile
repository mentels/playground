.PHONY: run clean

all: assertions.beam run

assertions.beam: assertions.erl
	erlc assertions.erl

run:
	erl -noshell -eval "eunit:test(assertions,[{verbose,true}])" -s init stop

clean:
	rm *.beam
