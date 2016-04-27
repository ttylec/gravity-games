LIBS=vectors.elm spacesim.elm

all: mars shooter tidial

mars: mars.elm $(LIBS)
	elm-make mars.elm --output mars.js

shooter: shooter.elm $(LIBS)
	elm-make shooter.elm --output shooter.js

tidial: tidial.elm $(LIBS)
	elm-make tidial.elm --output tidial.js

clean:
	rm -f *.js
	rm -rf elm-stuff
