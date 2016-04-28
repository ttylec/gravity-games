LIBS=vectors.elm spacesim.elm

all: mars shooter tidial

mars: mars.elm $(LIBS)
	elm-make mars.elm --output mars.js

shooter: shooter.elm $(LIBS)
	elm-make shooter.elm --output shooter.js

tidial: tidal.elm $(LIBS)
	elm-make tidal.elm --output tidal.js

clean:
	rm -f *.js
	rm -rf elm-stuff
