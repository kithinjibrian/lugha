HOME="."
TRANSPILER="lugha"

generate() {
    bison -d -Wcounterexamples ./src/parser/gens/bison.y
    mv bison.tab.h ./include
    mv bison.tab.c ./src/parser/gens

    flex ./src/parser/gens/flex.l
    mv lex.yy.c ./src/parser/gens
}

build() {
	echo "Building $TRANSPILER"
    generate
	make -C "$HOME"
    echo -e "\n\n\n"
}

clean() {
    rm -rf ./src/parser/gens/bison.tab.c ./src/parser/gens/lex.yy.c ./include/bison.tab.h
	make -C "$HOME" clean
}

if [ "$1" == "clean" ]; then
	clean

elif [ "$1" == "run" ]; then
	build
	./lugha

elif [ "$1" == "force-run" ]; then
	clean
	build
	./lugha main.lg -o main.js -l js

elif [ "$1" == "debug" ]; then
	clean
	build
	gdb ./lugha

elif [ "$1" == "valgrind" ]; then
	clean
	build
	valgrind --leak-check=full --show-reachable=yes --track-origins=yes ./lugha main.lg
	
else
	build

fi
