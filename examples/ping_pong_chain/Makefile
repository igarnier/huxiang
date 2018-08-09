all: clean
	jbuilder build ping.exe pong.exe
	ln -s _build/default/ping.exe ./ping
	ln -s _build/default/pong.exe ./pong

clean:
	rm -f ping pong
	jbuilder clean
