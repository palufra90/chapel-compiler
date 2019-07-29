all:
	happy -gca Parchapel.y
	alex -g Lexchapel.x
	ghc --make Testchapel.hs -o Testchapel

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bak 
	-rm -f Docchapel.ps
	-rm -f Testchapel

demo1: 
	./Testchapel ./script/es1.chpl
demo2: 
	./Testchapel ./script/es2.chpl
demo3: 
	./Testchapel ./script/es3.chpl
demo4: 
	./Testchapel ./script/es4.chpl

