all: clean
	jbuilder build pp_ping.exe pp_pong.exe pp_evil.exe oracle.exe
	ln -s _build/default/pp_ping.exe ./pp_ping
	ln -s _build/default/pp_pong.exe ./pp_pong
	ln -s _build/default/pp_evil.exe ./pp_evil
	ln -s _build/default/oracle.exe ./oracle

clean:
	rm -f pp_ping pp_pong pp_evil oracle
	jbuilder clean
