.PHONY: all server client test

all: server client
 
server: 
	$(MAKE) -C server all

client: 
	$(MAKE) -C client all

test:
	rebar3 eunit -m 'test_main@ps' -v
