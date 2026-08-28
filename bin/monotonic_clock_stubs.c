/*******************************************************************************
 *                                                                             *
 * Copyright (c) 2026 Epure Team                                               *
 * All rights reserved.                                                        *
 *                                                                             *
 *******************************************************************************/

#define CAML_NAME_SPACE

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <time.h>

CAMLprim value cabal_monotonic_clock_now(value unit)
{
  CAMLparam1(unit);
  struct timespec now;

  if (clock_gettime(CLOCK_MONOTONIC, &now) != 0)
    caml_failwith("clock_gettime(CLOCK_MONOTONIC) failed");

  CAMLreturn(caml_copy_double((double)now.tv_sec + (double)now.tv_nsec / 1e9));
}
