# Change these options to match the location of your libyaml installation:
YAMLINCDIR = /usr/local/include
YAMLLIBDIR = /usr/local/lib

CFLAGS = -I$(YAMLINCDIR)
LDFLAGS = -L$(YAMLLIBDIR) -lyaml

OBJS = psdf-body.o

input_output: $(OBJS)

.PHONY: clean
clean:
	rm *.o
	rm input_output