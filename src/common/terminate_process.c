#include <caml/mlvalues.h>
#include <caml/memory.h>

// #define _WIN32_WINNT 0x0501
#include <windows.h>
#include <strsafe.h>

CAMLprim value o_terminate_process(value o_pid)
{
  CAMLparam1(o_pid);
  CAMLlocal1(o_result);

  HANDLE pid = (HANDLE) Long_val(o_pid);
  BOOL result = TerminateProcess(pid, 137); 
  o_result = Val_bool(result);

  CAMLreturn(o_result);
}