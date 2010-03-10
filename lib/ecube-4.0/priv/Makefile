ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.6.4
CFLAGS=-O3 -Wall -I/usr/local/include -I$(ERL_LIB)/include
LDFLAGS=-L. -L$(ERL_LIB)/lib `pkg-config libpulse-simple --libs` -lerl_interface -lei -lpthread

all: rec

rec: rec.c
	gcc $(CFLAGS) $< -o $@ $(LDFLAGS)

clean:
	@rm -f *~ rec
