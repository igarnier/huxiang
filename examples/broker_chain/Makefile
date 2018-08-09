all: clean
	jbuilder build mother.exe client.exe sb_brok.exe sb_serv.exe oracle.exe
	ln -s _build/default/mother.exe ./mother
	ln -s _build/default/client.exe ./client
	ln -s _build/default/sb_brok.exe ./sb_brok
	ln -s _build/default/sb_serv.exe ./sb_serv
	ln -s _build/default/oracle.exe ./oracle

clean:
	rm -f mother client sb_brok sb_serv oracle
	jbuilder clean
