#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <assert.h>
#include <stdio.h>
#include <string.h>

CAMLprim value o_terminate_process(value o_pid)
{
  CAMLparam1(o_pid);
  CAMLlocal1(o_result);

  o_result = Val_bool(0);

  CAMLreturn(o_result);
}