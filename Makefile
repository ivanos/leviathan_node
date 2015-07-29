.PHONY: all compile get-deps update-deps clean deep-clean rel

all: rebar rel

compile: get-deps
	./rebar compile

get-deps:
	./rebar get-deps

update-deps:
	./rebar update-deps

clean:
	./rebar clean

deep-clean: clean
	rm -rf deps/*/ebin/*

rel: compile id_rsa
	./relx -c _rel/relx.config

rebar:
	wget -c https://github.com/rebar/rebar/wiki/rebar
	chmod +x rebar

id_rsa:
	deps/erl_sshd/make_keys
