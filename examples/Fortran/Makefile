# Change these options to match the location of your libyaml installation:
YAMLINCDIR = /usr/local/include
YAMLLIBDIR = /usr/local/lib

CFLAGS = -I$(YAMLINCDIR) -I../C
LDFLAGS = -L$(YAMLLIBDIR) -lyaml

OBJS = psdf-body-fint.o psdf-ftest.o ../C/psdf-body.o

psdf-ftest: $(OBJS)
	f77 -o psdf-ftest $(OBJS) $(LDFLAGS)

.PHONY: clean
clean:
	rm *.o
	rm ftest