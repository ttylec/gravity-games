LIBS=vectors.elm spacesim.elm

all: mars shooter

mars: mars.elm $(LIBS)
	elm-make mars.elm --output mars.js

shooter: shooter.elm $(LIBS)
	elm-make shooter.elm --output shooter.js

clean:
	rm -f *.js
	rm -f elm-package.json
	rm -rf elm-stuff
