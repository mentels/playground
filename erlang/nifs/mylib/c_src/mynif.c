#include <erl_nif.h>
#include <stdlib.h>
#include <stdatomic.h>

// DOCs:
// * http://erlang.org/doc/man/erl_nif.html
// * http://erlang.org/doc/reference_manual/code_loading.html#running-a-function-when-a-module-is-loaded

// global variable accessible from the whole BEAM
/* int global_counter = 0; */
atomic_int global_counter = 0;
ErlNifEnv *indp_env;
ERL_NIF_TERM ok_atom;

/* BEGIN our own resource */

typedef struct {
  unsigned size;
  int *data;
} array_t;

ErlNifResourceType *array_res_type;

void deinit_array(ErlNifEnv *env, void *obj) {
  array_t *array = (array_t*) obj;
  free(array->data);
}

// new is an explicit name
ERL_NIF_TERM new(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {
  array_t *arr = (array_t*) enif_alloc_resource(array_res_type, sizeof(array_t));

  arr->size = 128;
  arr->data = malloc(128 * sizeof(int));

  ERL_NIF_TERM ret = enif_make_resource(env, arr);
  enif_release_resource(arr);
  return ret;
}

ERL_NIF_TERM resize(ErlNifEnv *env, int arg, const ERL_NIF_TERM *argv) {
  array_t *arr;
  if (!enif_get_resource(env, argv[0], array_res_type, (void**)&arr))
    return enif_make_badarg(env);

  unsigned int size;
  if (!enif_get_uint(env, argv[1], &size))
    return enif_make_badarg(env);

  // was `size` instead of `size*sizeof(int)`
  arr->data = realloc(arr->data, size * sizeof(int));
  arr->size = size;

  return enif_make_copy(env, ok_atom);
}

ERL_NIF_TERM shuffle(ErlNifEnv *env, int arg, const ERL_NIF_TERM *argv) {
  // get array
  array_t *arr;
  if (!enif_get_resource(env, argv[0], array_res_type, (void**)&arr))
    return enif_make_badarg(env);

  // get size
  unsigned int size = arr->size;

  int i,j;
  for(i=0; i<=size-2; ++i) {
    j = (rand() % (size-i))+i;
    int tmp = arr->data[i];
    arr->data[i] = arr->data[j];
    arr->data[j] = tmp;
  }

  return enif_make_copy(env, ok_atom);
}

/* END our own resource */

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  // create global atom not to lock on atom table every time  we create a new atom
  indp_env = enif_alloc_env();
  ok_atom = enif_make_atom(indp_env, "ok");
  array_res_type = enif_open_resource_type(env, NULL, "mynif.array_t", &deinit_array,
                                            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
  return 0;
}

// env: environment of the process calling the NIF
// http://erlang.org/doc/man/erl_nif.html#data-types
ERL_NIF_TERM hello_world(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {
  // argc numer of args from Erlang
  // arvg the list of args from Erlang
  return enif_make_atom(env, "hello_world");
}

ERL_NIF_TERM counter_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {
  // https://en.cppreference.com/w/c/atomic/atomic_load
  return enif_make_int(env, atomic_load(&global_counter));
}

ERL_NIF_TERM counter_inc(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {
  atomic_fetch_add(&global_counter, 1);
  /* return enif_make_atom(env, "ok"); */
  return enif_make_copy(env, ok_atom);
}

ERL_NIF_TERM counter_add(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {
  int add;
  // enif_term to C type
  enif_get_int(env, argv[0], &add);
  // https://en.cppreference.com/w/c/atomic/atomic_fetch_add
  atomic_fetch_add(&global_counter, add);
  /* return enif_make_atom(env, "ok"); */
  return enif_make_copy(env, ok_atom);
}

// declaration of exported functions
ErlNifFunc nif_funcs[] = {
                          // {the name of the function see from Erlang, number of args, corresponding function in C}
                          {"hello_world", 0, hello_world},
                          {"counter_get", 0, counter_get},
                          {"counter_inc", 0, counter_inc},
                          {"counter_add", 1, counter_add},
                          {"new", 0, new},
                          {"resize", 2, resize},
                          // w/o DIRTY SCHEDULERs the NIFs won't block the entire VM
                          {"shuffle", 1, shuffle, ERL_NIF_DIRTY_JOB_CPU_BOUND}

};

// a must for all NIFs
// load: callback called on loading the NIF
ERL_NIF_INIT(mynif, nif_funcs, load, NULL, NULL, NULL)

/* For version with mutex
// remember to pass the load function as 3rd arg to ERL_NIF_INIT
ErlNifMutex *mutex;

int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  mutex = enif_mutex_create("my_mutex");
  return 0; // loading fails if this function returns non-0
}

ERL_NIF_TERM counter_add(ErlNifEnv *env, int argc, const ERL_NIF_TERM *argv) {
  int add;
  // enif_term to C type
  enif_get_int(env, argv[0], &add);
  enif_mutex_lock(mutex);
  global_counter = global_counter + add;
  enif_mutex_unlock(mutex);
  return enif_make_atom(env, "ok");
}
*/
