all: clean
	jbuilder build ping.exe pong.exe evilPing.exe
	ln -s _build/default/ping.exe ./ping
	ln -s _build/default/pong.exe ./pong
	ln -s _build/default/evilPing.exe ./evilPing

clean:
	rm -f ping pong evilPing
	jbuilder clean
