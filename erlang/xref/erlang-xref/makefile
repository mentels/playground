.PHONY: deps compile test

all: deps compile test

deps:
	git clone git@github.com:inaka/xref_runner.git
	cd xref_runner && rebar3 compile

compile:
	ercl *.erl

test:
	erl -pa xref_runner/_build/default/lib/xref_runner/ebin/ \
	-eval "eunit:test(xref_test, [verbose])." \
	-noshell \
	-s init stop

shell:
	erl -pa xref_runner/_build/default/lib/xref_runner/ebin/

clean:
	rm *.beam
