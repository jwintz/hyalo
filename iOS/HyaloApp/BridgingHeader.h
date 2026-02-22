//
// Bridging Header for Hyalo iOS
//

#ifndef BridgingHeader_h
#define BridgingHeader_h

// Emacs iOS entry point from libemacs.a
int ios_emacs_init(void);

// Get fingerprint for pdmp matching
const char *ios_get_fingerprint(void);

// Bootstrap callbacks
void ios_notify_bootstrap_start(void);
void ios_report_load_progress(const char *filename);
void ios_report_bootstrap_complete(void);

#endif /* BridgingHeader_h */
