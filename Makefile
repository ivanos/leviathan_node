.PHONY: all compile dev_compile dev_compile2 get-deps update-deps \
	clean deep-clean rel proper

all: rebar rel

compile: get-deps
	./rebar compile

dev_compile: deep-clean get-deps
	LEV_DEV=true $(MAKE) dev_compile2

dev_compile2:
	./rebar compile -D TEST apps=lager,lucet,leviathan_lib,leviathan
	./rebar compile


get-deps:
	./rebar get-deps

update-deps:
	./rebar update-deps

proper: dev_compile no_compile_proper

no_compile_proper: 
	rm -rf Mnesia.prop_test@*
	erl -pa ebin deps/*/ebin \
	-noshell \
	-sname prop_test -erl_mnesia options \[persistent\] \
	-eval "R = [begin \
		{_, [M]}=re:run(P, \".*/(?<m>.*).beam\", [{capture, ['m'], list}]), \
		proper:module(list_to_atom(M)) end || P <- filelib:wildcard(\"ebin/prop_*\")], \
		R == [] andalso io:format(\"No properties to run.~n\")" \
	 -s init stop

clean:
	./rebar clean

deep-clean: clean
	rm -f deps/*/ebin/*

rel: compile id_rsa
	./relx -c _rel/relx.config

rebar:
	wget -c https://github.com/rebar/rebar/wiki/rebar
	chmod +x rebar

id_rsa:
	deps/erl_sshd/make_keys

tls:
	deps/erl_cowboy/make_tls.sh
