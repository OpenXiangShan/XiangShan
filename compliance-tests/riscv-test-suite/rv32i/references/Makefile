SRC=$(wildcard *.reference_output)
OBJ=$(patsubst %.reference_output,%.reference_output_n,$(SRC))

all:$(OBJ)
	@echo -e '/*\nReferences are ready!\n*/'

$(OBJ):%.reference_output_n:%.reference_output
	@sed -n '1,32p' $^ > $@

clean:
	rm -f *.reference_output_n
