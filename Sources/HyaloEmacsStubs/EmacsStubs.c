/* EmacsStubs.c - Bridge stubs for Hyalo-specific symbols.
   These provide no-op implementations for symbols that are defined by
   the Hyalo Swift bridge (@_cdecl) but also referenced by libemacs.a.
   On device/simulator, libemacs.a provides the Emacs entry points
   (ios_emacs_init, ios_signal_event_available, etc.) — they are NOT
   stubbed here.

   Only symbols that originate from the Hyalo bridge and are not part
   of libemacs.a belong in this file. */

#include <stdint.h>
#include <stddef.h>

/* Dispatch command from Swift to Emacs.
   Provided by ChannelBridge.swift via @_cdecl at link time.
   This stub is only used when the bridge is not linked. */
void
hyalo_ios_dispatch_command (int command_id, const char *json_payload)
{
  (void)command_id;
  (void)json_payload;
}

/* Receive Swift response from reverse channel.
   Provided by ChannelBridge.swift via @_cdecl at link time.
   This stub is only used when the bridge is not linked. */
void
hyalo_ios_receive_swift_response (const char *json_response)
{
  (void)json_response;
}
