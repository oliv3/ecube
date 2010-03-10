#include <stdio.h>
#include <stdlib.h>
#include <pulse/simple.h>
#include <pulse/error.h>
#include <ei.h>
#include <unistd.h>
#include <string.h>

#define INSIZE	   256
#define ABUFF_SIZE INSIZE * 1 /* channels */  * sizeof(float)


static float pa_buff[ABUFF_SIZE];
static pa_simple *pa_s = NULL;
static pthread_t recorder;
static int recording = 0;

/*
 * PulseAudio-based recorder
 *
 * Protocol: {packet, 4}  %% on pourrait faire du {packet, 2} mais flemme
 *                        %% de modifier du code qui marche
 *
 * Commands:
 * 
 * {start, Frequency}
 * stop
 *
 */

//#include <pthread.h>
//#include <arpa/inet.h>
//#include <sys/io.h>

#define DEBUG
#define BUF_SIZE 65535

static uint32_t read_cmd(char *buf, uint32_t *size);
static uint32_t write_cmd(ei_x_buff* x);
static uint32_t read_exact(char *buf, uint32_t len);
static uint32_t write_exact(char *buf, uint32_t len);

#ifdef DEBUG
#define D(F, A) do { fprintf(stderr, F "\r\n", A); fflush(stderr); } while (0)
#else
#define D(F, A) {}
#endif


static inline void
check(int val) {
  if (-1 == val) {
    /* we don't free anything, exiting anyway */
    exit(1);
  }
}


/*
static int
my_decode_double(char *buf, int *index, double *val) {
  long l;
  unsigned long ul;
  long long ll;
  unsigned long long ull;

  if (!ei_decode_double(buf, index, val)) return 0;
  if (!ei_decode_long(buf, index, &l)) { *val = l; return 0; }
  if (!ei_decode_ulong(buf, index, &ul)) { *val = ul; return 0; }
  if (!ei_decode_longlong(buf, index, &ll)) { *val = ll; return 0; }
  if (!ei_decode_ulonglong(buf, index, &ull)) { *val = ull; return 0; }

  return -1;
}
*/


static void
ok() {
  ei_x_buff result;

  check(ei_x_new_with_version(&result));
  check(ei_x_encode_atom(&result, "ok"));

  write_cmd(&result);
  ei_x_free(&result);
}


static void
error() {
  ei_x_buff result;

  check(ei_x_new_with_version(&result));
  check(ei_x_encode_tuple_header(&result, 2));
  check(ei_x_encode_atom(&result, "error"));
  if (recording)
    check(ei_x_encode_atom(&result, "already_started"));
  else
    check(ei_x_encode_atom(&result, "not_started"));
  
  write_cmd(&result);
  ei_x_free(&result);
}


static void
start_recording(long frequency) {
  recording = 1;

  ok();
}


static void
stop_recording() {
  recording = 0;

  ok();
}


int
main(int argc, char **argv) {
  char     *buf = NULL;
  uint32_t size = BUF_SIZE;
  char     command[MAXATOMLEN];
  int      index, version;

  if ((buf = (char *)calloc(size, sizeof(char))) == NULL)
    return -1;

  while (read_cmd(buf, &size) > 0) {
    /* Reset the index, so that ei functions can decode terms from the 
     * beginning of the buffer */
    index = 0;

    D("buf: %s", buf);
    
    /* Ensure that we are receiving the binary term by reading and 
     * stripping the version byte */
    check(ei_decode_version(buf, &index, &version));

    /* Ici donc le code du recorder:
     *
     * if decode_atom => stop:
     *   arreter le thread -> ok, {error, not_started} sinon
     * else
     * if decode tuple de taille 2:
     *   starter le thread -> ok, {error, already_started} sinon
     */
    if (!ei_decode_atom(buf, &index, command)) {
      D("got atom: %s", command);
      if (!strcmp(command, "stop")) {
	if (recording)
	  stop_recording();
	else
	  error();
      } else
	check(-1);
    } else {
      int arity;
      long frequency;

      check(ei_decode_tuple_header(buf, &index, &arity));
      // D("ARITY: %d", arity);
      if (arity != 2) check(-1);

      check(ei_decode_atom(buf, &index, command));
      // D("got atom 2: %s", command);
      if (strcmp(command, "record")) check(-1);

      check(ei_decode_long(buf, &index, &frequency));
      // D("FREQ: %li", frequency);

      if (!recording) {
	start_recording(frequency);
      } else
	error();
    }

    memset(buf, 0, size * sizeof(char));
  }

  free(buf);

  return 0;
}


/*-----------------------------------------------------------------
 * Data marshalling functions
 *----------------------------------------------------------------*/
static uint32_t
read_cmd(char *buf, uint32_t *size) {
  uint32_t plen, len;

  if (read_exact((char *)&plen, 4) != 4)
    return(-1);

  len = ntohl(plen);
  D("Reading %d bytes", len);

  return read_exact(buf, len);
}


static uint32_t
write_cmd(ei_x_buff *buff) {
  uint32_t len = buff->index;
  uint32_t plen;

  plen = htonl(len);
  write_exact((char *)&plen, 4);

  return write_exact(buff->buff, len);
}


static uint32_t
read_exact(char *buf, uint32_t len) {
  uint32_t i, got = 0;

  do {
    if ((i = read(0, buf+got*sizeof(char), (len-got)*sizeof(char))) <= 0)
      return i;
    got += i;
  } while (got <len);

  return len;
}


static uint32_t
write_exact(char *buf, uint32_t len) {
  uint32_t i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);

  return len;
}




#if 0
/* really old code */

    check(ei_decode_tuple_header(buf, &index, &arity));
    
    if (arity != 3) return 3;
  
    check(ei_decode_atom(buf, &index, command));
    
    if (!strcmp("curve", command)) {
      int i, l_arity;
      ei_x_buff result;
      
      check(ei_decode_long(buf, &index, &span));
      check(ei_decode_list_header(buf, &index, &l_arity));
      D("List of arity %d", l_arity);
	
      for (i = 0; i < l_arity; i++) {
	int t_arity;
	  
	check(ei_decode_tuple_header(buf, &index, &t_arity));
	D("Tuple of arity %d", t_arity);
	if (t_arity == 3) {
	  double x, y, z;
	  check(my_decode_double(buf, &index, &x));
	  check(my_decode_double(buf, &index, &y));
	  check(my_decode_double(buf, &index, &z));
	} else
	  check(-1);
      }
      /* skip empty list tail */
      ei_skip_term(buf, &index);
      D("Got %d points", l_arity);

      /* Prepare the output buffer that will hold the result */
      check(ei_x_new_with_version(&result));
      check(ei_x_encode_list_header(&result, sp->nb_spoints));

      for (i = 0; i < sp->nb_spoints; i++) {
	check(ei_x_encode_tuple_header(&result, 3));
	check(ei_x_encode_double(&result, sp->spoints[i].coords[0]));
	check(ei_x_encode_double(&result, sp->spoints[i].coords[1]));
	check(ei_x_encode_double(&result, sp->spoints[i].coords[2]));
      }
      check(ei_x_encode_empty_list(&result));
      
      write_cmd(&result);
      ei_x_free(&result);
      
      memset(buf, 0, size*sizeof(char));
    } else
      check(-1);
#endif /* 0 */
