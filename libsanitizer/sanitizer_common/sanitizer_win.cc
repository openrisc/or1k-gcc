//===-- sanitizer_win.cc --------------------------------------------------===//
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file is shared between AddressSanitizer and ThreadSanitizer
// run-time libraries and implements windows-specific functions from
// sanitizer_libc.h.
//===----------------------------------------------------------------------===//
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOGDI
#include <stdlib.h>
#include <windows.h>

#include "sanitizer_common.h"
#include "sanitizer_libc.h"

namespace __sanitizer {

// --------------------- sanitizer_common.h
uptr GetPageSize() {
  return 1U << 14;  // FIXME: is this configurable?
}

uptr GetMmapGranularity() {
  return 1U << 16;  // FIXME: is this configurable?
}

bool FileExists(const char *filename) {
  UNIMPLEMENTED();
}

int GetPid() {
  return GetProcessId(GetCurrentProcess());
}

uptr GetThreadSelf() {
  return GetCurrentThreadId();
}

void GetThreadStackTopAndBottom(bool at_initialization, uptr *stack_top,
                                uptr *stack_bottom) {
  CHECK(stack_top);
  CHECK(stack_bottom);
  MEMORY_BASIC_INFORMATION mbi;
  CHECK_NE(VirtualQuery(&mbi /* on stack */, &mbi, sizeof(mbi)), 0);
  // FIXME: is it possible for the stack to not be a single allocation?
  // Are these values what ASan expects to get (reserved, not committed;
  // including stack guard page) ?
  *stack_top = (uptr)mbi.BaseAddress + mbi.RegionSize;
  *stack_bottom = (uptr)mbi.AllocationBase;
}

void *MmapOrDie(uptr size, const char *mem_type) {
  void *rv = VirtualAlloc(0, size, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (rv == 0) {
    Report("ERROR: Failed to allocate 0x%zx (%zd) bytes of %s\n",
           size, size, mem_type);
    CHECK("unable to mmap" && 0);
  }
  return rv;
}

void UnmapOrDie(void *addr, uptr size) {
  if (VirtualFree(addr, size, MEM_DECOMMIT) == 0) {
    Report("ERROR: Failed to deallocate 0x%zx (%zd) bytes at address %p\n",
           size, size, addr);
    CHECK("unable to unmap" && 0);
  }
}

void *MmapFixedNoReserve(uptr fixed_addr, uptr size) {
  void *p = VirtualAlloc((LPVOID)fixed_addr, size,
      MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (p == 0)
    Report("ERROR: Failed to allocate 0x%zx (%zd) bytes at %p (%d)\n",
           size, size, fixed_addr, GetLastError());
  return p;
}

void *Mprotect(uptr fixed_addr, uptr size) {
  return VirtualAlloc((LPVOID)fixed_addr, size,
                      MEM_RESERVE | MEM_COMMIT, PAGE_NOACCESS);
}

bool MemoryRangeIsAvailable(uptr range_start, uptr range_end) {
  // FIXME: shall we do anything here on Windows?
  return true;
}

void *MapFileToMemory(const char *file_name, uptr *buff_size) {
  UNIMPLEMENTED();
}

const char *GetEnv(const char *name) {
  static char env_buffer[32767] = {};

  // Note: this implementation stores the result in a static buffer so we only
  // allow it to be called just once.
  static bool called_once = false;
  if (called_once)
    UNIMPLEMENTED();
  called_once = true;

  DWORD rv = GetEnvironmentVariableA(name, env_buffer, sizeof(env_buffer));
  if (rv > 0 && rv < sizeof(env_buffer))
    return env_buffer;
  return 0;
}

const char *GetPwd() {
  UNIMPLEMENTED();
}

void DumpProcessMap() {
  UNIMPLEMENTED();
}

void DisableCoreDumper() {
  UNIMPLEMENTED();
}

void ReExec() {
  UNIMPLEMENTED();
}

bool StackSizeIsUnlimited() {
  UNIMPLEMENTED();
}

void SetStackSizeLimitInBytes(uptr limit) {
  UNIMPLEMENTED();
}

void SleepForSeconds(int seconds) {
  Sleep(seconds * 1000);
}

void SleepForMillis(int millis) {
  Sleep(millis);
}

void Exit(int exitcode) {
  _exit(exitcode);
}

void Abort() {
  abort();
  _exit(-1);  // abort is not NORETURN on Windows.
}

#ifndef SANITIZER_GO
int Atexit(void (*function)(void)) {
  return atexit(function);
}
#endif

// ------------------ sanitizer_libc.h
void *internal_mmap(void *addr, uptr length, int prot, int flags,
                    int fd, u64 offset) {
  UNIMPLEMENTED();
}

int internal_munmap(void *addr, uptr length) {
  UNIMPLEMENTED();
}

int internal_close(fd_t fd) {
  UNIMPLEMENTED();
}

int internal_isatty(fd_t fd) {
  UNIMPLEMENTED();
}

fd_t internal_open(const char *filename, bool write) {
  UNIMPLEMENTED();
}

uptr internal_read(fd_t fd, void *buf, uptr count) {
  UNIMPLEMENTED();
}

uptr internal_write(fd_t fd, const void *buf, uptr count) {
  if (fd != kStderrFd)
    UNIMPLEMENTED();
  HANDLE err = GetStdHandle(STD_ERROR_HANDLE);
  if (err == 0)
    return 0;  // FIXME: this might not work on some apps.
  DWORD ret;
  if (!WriteFile(err, buf, count, &ret, 0))
    return 0;
  return ret;
}

uptr internal_filesize(fd_t fd) {
  UNIMPLEMENTED();
}

int internal_dup2(int oldfd, int newfd) {
  UNIMPLEMENTED();
}

uptr internal_readlink(const char *path, char *buf, uptr bufsize) {
  UNIMPLEMENTED();
}

int internal_sched_yield() {
  Sleep(0);
  return 0;
}

}  // namespace __sanitizer

#endif  // _WIN32
