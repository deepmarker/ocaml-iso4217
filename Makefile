all:
	dune build @runtest @install

download:
	curl -o lib/list-one.xml https://www.six-group.com/dam/download/financial-information/data-center/iso-currrency/lists/list-one.xml

clean:
	dune clean

