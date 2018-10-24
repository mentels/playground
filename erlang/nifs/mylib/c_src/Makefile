# get the paths for the headers
ERLANG_PATH = $(shell erl -eval 'io:format("~s", [lists:concat([code:root_dir(), "/erts-", erlang:system_info(version), "/include"])])' -s init stop -noshell)

# -fPIC: build position independent (must for shared libraries on Linux)
# -g: for debug
# -I: for erlang headers
# -03: for optimisations
CFLAGS += -fPIC -g -std=c11 -I$(ERLANG_PATH)

ifeq ($(shell uname),Darwin)
# for undefined functions allow for binding to unkwnon functions at compile time
# look up for them at runtime
# e.g. at compile time return_enif_atom implementation is unknown
    LDFLAGS += -undefined dynamic_lookup
endif

../priv/mynif.so: mynif.c
	mkdir -p ../priv
	gcc -shared $(CFLAGS) $(LDFLAGS) -o $@ $<
