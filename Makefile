pi:  pi.ml 
	ocamlc -o pi pi.ml

clean:
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f pi 
