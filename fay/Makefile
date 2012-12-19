all: app

FAYC=~/.cabal/bin/fay
FAYFLAGS=-p --flatten-apps --include ./src

demo:
	${FAYC} ${FAYFLAGS} demo.hs

app:
	${FAYC} ${FAYFLAGS} src/app.hs -o app.js

help:
	${FAYC} -h

clean:
	rm demo.js app.js
