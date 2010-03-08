ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.4
CFLAGS=-O3 -Wall -I/usr/local/include -I$(ERL_LIB)/include
LDFLAGS=-L. -L$(ERL_LIB)/lib

all: rec

rec: rec.c
	gcc $(CFLAGS) $< -o $@ $(LDFLAGS) -lerl_interface -lei -lpthread

clean:
	@rm -f *~ rec
