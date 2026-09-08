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
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
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

#if defined(__linux__) || defined(__APPLE__)
static int cabal_remove_directory_contents(int directory)
{
  int stream_descriptor = dup(directory);
  DIR *stream;
  struct dirent *entry;

  if (stream_descriptor == -1) return -1;
  stream = fdopendir(stream_descriptor);
  if (stream == NULL) {
    close(stream_descriptor);
    return -1;
  }

  errno = 0;
  while ((entry = readdir(stream)) != NULL) {
    struct stat metadata;
    int result;

    if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
      continue;
    do {
      result = fstatat(directory, entry->d_name, &metadata, AT_SYMLINK_NOFOLLOW);
    } while (result == -1 && errno == EINTR);
    if (result == -1) {
      closedir(stream);
      return -1;
    }
    if (S_ISDIR(metadata.st_mode)) {
      int flags = O_RDONLY | O_DIRECTORY | O_NOFOLLOW;
      int child;
#ifdef O_CLOEXEC
      flags |= O_CLOEXEC;
#endif
      do {
        child = openat(directory, entry->d_name, flags);
      } while (child == -1 && errno == EINTR);
      if (child == -1 || cabal_remove_directory_contents(child) == -1) {
        if (child != -1) close(child);
        closedir(stream);
        return -1;
      }
      close(child);
      do {
        result = unlinkat(directory, entry->d_name, AT_REMOVEDIR);
      } while (result == -1 && errno == EINTR);
    } else {
      do {
        result = unlinkat(directory, entry->d_name, 0);
      } while (result == -1 && errno == EINTR);
    }
    if (result == -1) {
      closedir(stream);
      return -1;
    }
    errno = 0;
  }
  if (errno != 0) {
    closedir(stream);
    return -1;
  }
  return closedir(stream);
}
#endif

CAMLprim value cabal_secure_remove_tree(value path)
{
  CAMLparam1(path);

#if defined(__linux__) || defined(__APPLE__)
  int flags = O_RDONLY | O_DIRECTORY | O_NOFOLLOW;
  int directory;
  struct stat opened;
  struct stat current;
  int result;

  caml_unix_check_path(path, "secure_remove_tree");
#ifdef O_CLOEXEC
  flags |= O_CLOEXEC;
#endif
  do {
    directory = open(String_val(path), flags);
  } while (directory == -1 && errno == EINTR);
  if (directory == -1) {
    if (errno == ENOENT) CAMLreturn(Val_true);
    CAMLreturn(Val_false);
  }
  if (fstat(directory, &opened) == -1 ||
      cabal_remove_directory_contents(directory) == -1) {
    close(directory);
    CAMLreturn(Val_false);
  }
  close(directory);
  do {
    result = lstat(String_val(path), &current);
  } while (result == -1 && errno == EINTR);
  if (result == -1) {
    if (errno == ENOENT) CAMLreturn(Val_true);
    CAMLreturn(Val_false);
  }
  if (!S_ISDIR(current.st_mode) || current.st_dev != opened.st_dev ||
      current.st_ino != opened.st_ino)
    CAMLreturn(Val_false);
  do {
    result = rmdir(String_val(path));
  } while (result == -1 && errno == EINTR);
  CAMLreturn(Val_bool(result == 0 || errno == ENOENT));
#else
  CAMLreturn(Val_false);
#endif
}
