elm.js : src/Main.elm
	elm make src/Main.elm --optimize

.PHONY: clean
clean:
	rm -rf ./elm-stuff index.html

.PHONY: live
live:
	elm-live -h 0.0.0.0 src/Main.elm --debug