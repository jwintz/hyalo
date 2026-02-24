/* EmacsStubs.c - Simulator stubs for libemacs C entry points.
   These provide no-op implementations when building for the iOS Simulator,
   where libemacs.a is not available. On device, libemacs.a provides the real
   implementations and these stubs are not linked. */

#include <stdint.h>
#include <stddef.h>

/* Dispatch command from Swift to Emacs.
   This symbol is NOT in libemacs.a and must be provided here.
   No-op under the simulator. */
void
hyalo_ios_dispatch_command (int command_id, const char *json_payload)
{
  (void)command_id;
  (void)json_payload;
}

/* Receive Swift response from reverse channel.
   This symbol is NOT in libemacs.a and must be provided here.
   No-op under the simulator. */
void
hyalo_ios_receive_swift_response (const char *json_response)
{
  (void)json_response;
}
