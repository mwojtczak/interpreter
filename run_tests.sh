#!/bin/bash -eu
make
echo "----------TESTY W KATALOGU TESTS/GOOD----------"
for program in tests/good/*.mil; do
	echo "$program" 
 	./interpreter "$program" > temp.out
	diff temp.out "${program:0:-4}.out"
	rm temp.out
done

echo "----------TESTY W KATALOGU TESTS/BAD-----------"
for program in tests/bad/*.mil; do
	echo "$program" 
 	./interpreter "$program" > temp.out
	cat temp.out
	rm temp.out
done

