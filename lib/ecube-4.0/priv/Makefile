ERL_LIB=/usr/local/lib/erlang/lib/erl_interface-3.7.3
CFLAGS=-O3 -Wall -I/usr/local/include -I$(ERL_LIB)/include -DHAVE_CONFIG_H
CFLAGS+=`pkg-config glib-2.0 --cflags`
LDFLAGS=-L. -L$(ERL_LIB)/lib `pkg-config libpulse-simple --libs` -lerl_interface -lei -lpthread

all: rec

# rec: rec.c spline.c
# 	gcc $(CFLAGS) $< -o $@ $(LDFLAGS)

OBJS=rec.o utils.o spline.o
rec: $(OBJS)
	gcc $(OBJS) $(LDFLAGS) -o $@ -lerl_interface -lei -lpthread 

%.o: %.c
	gcc $(DEBUG) -c $(CFLAGS) $< -o $@

clean:
	@rm -f *.o *~ rec
