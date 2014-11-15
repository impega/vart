EXEC := Mondrian

all:
	ghc --make Main -o $(EXEC)

dev:
	watch -d -n 1 'ghc -fno-code Main.hs'

clean:
	rm -f $(EXEC) *.o *.hi *~ *.png
