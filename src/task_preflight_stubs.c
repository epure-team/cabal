/*******************************************************************************
 *                                                                             *
 * Copyright (c) 2026 Epure Team                                               *
 * All rights reserved.                                                        *
 *                                                                             *
 *******************************************************************************/

#define CAML_NAME_SPACE

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <errno.h>

#if defined(__linux__) || defined(__APPLE__)
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <unistd.h>
#endif

CAMLprim value cabal_task_preflight_openat(value directory, value path)
{
  CAMLparam2(directory, path);

#if defined(__linux__) || defined(__APPLE__)
  int flags = O_RDONLY | O_NONBLOCK;
  int descriptor;

  caml_unix_check_path(path, "openat");
#ifdef O_CLOEXEC
  flags |= O_CLOEXEC;
#endif

  do {
    descriptor = openat(Int_val(directory), String_val(path), flags);
  } while (descriptor == -1 && errno == EINTR);

  if (descriptor == -1) caml_uerror("openat", path);

#ifndef O_CLOEXEC
  if (fcntl(descriptor, F_SETFD, FD_CLOEXEC) == -1) {
    int error = errno;
    close(descriptor);
    caml_unix_error(error, "openat", path);
  }
#endif

  CAMLreturn(Val_int(descriptor));
#else
  caml_unix_error(ENOSYS, "openat", path);
#endif
}

CAMLprim value cabal_task_preflight_descriptor_path(value descriptor)
{
  CAMLparam1(descriptor);

#if defined(__linux__)
  char link_path[64];
  char resolved[PATH_MAX + 1];
  int printed;
  ssize_t length;

  printed = snprintf(
      link_path, sizeof(link_path), "/proc/self/fd/%d", Int_val(descriptor));
  if (printed < 0 || (size_t)printed >= sizeof(link_path))
    caml_unix_error(ENAMETOOLONG, "readlink", Nothing);

  do {
    length = readlink(link_path, resolved, PATH_MAX);
  } while (length == -1 && errno == EINTR);

  if (length == -1) caml_uerror("readlink", Nothing);
  if (length >= PATH_MAX)
    caml_unix_error(ENAMETOOLONG, "readlink", Nothing);

  resolved[length] = '\0';
  CAMLreturn(caml_copy_string(resolved));
#elif defined(__APPLE__)
  char resolved[PATH_MAX];

  if (fcntl(Int_val(descriptor), F_GETPATH, resolved) == -1)
    caml_uerror("fcntl(F_GETPATH)", Nothing);

  resolved[PATH_MAX - 1] = '\0';
  CAMLreturn(caml_copy_string(resolved));
#else
  caml_unix_error(ENOSYS, "descriptor_path", Nothing);
#endif
}
