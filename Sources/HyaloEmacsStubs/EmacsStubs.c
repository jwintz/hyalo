/* EmacsStubs.c - Simulator stubs for libemacs C entry points.
   These provide no-op implementations when building for the iOS Simulator,
   where libemacs.a is not available. On device, libemacs.a provides the real
   implementations and these stubs are not linked. */

#include <stdint.h>
#include <stddef.h>

#if TARGET_OS_SIMULATOR

/* Emacs entry point: initialise Emacs with argc/argv and an optional dump file.
   Returns 0 on success. */
int
ios_emacs_init (int argc,
                char * _Nullable * _Nullable argv,
                const char * _Nullable dump_file)
{
  return 0;
}

/* Return the build fingerprint string used to locate the portable dump file.
   Returns NULL when running under the simulator (no real dump). */
char *
ios_get_fingerprint (void)
{
  return NULL;
}

/* Associate the primary UIWindow with the Emacs display layer.
   No-op under the simulator. */
void
ios_set_main_window (void *window)
{
  (void)window;
}

/* Signal Emacs that an input event is available.
   No-op under the simulator. */
void
ios_signal_event_available (void)
{
}

/* Bootstrap progress callbacks — no-op under the simulator. */
void ios_notify_bootstrap_start (void) {}
void ios_report_load_progress (const char *filename) { (void)filename; }
void ios_report_bootstrap_complete (void) {}

#endif /* TARGET_OS_SIMULATOR */
