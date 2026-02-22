#ifndef BridgingHeader_h
#define BridgingHeader_h

#import <UIKit/UIKit.h>

// Emacs iOS entry point
int ios_emacs_init(int argc, char **argv, const char *dump_file);
const char *ios_get_fingerprint(void);

// Bootstrap callbacks (called from libemacs into Swift via @_cdecl weak overrides)
void ios_notify_bootstrap_start(void);
void ios_report_load_progress(const char *filename);
void ios_report_bootstrap_complete(void);

// UIView handoff: Swift calls this to hand the UIWindow to libemacs before ios_emacs_init
void ios_set_main_window(UIWindow *window);

// ios_set_main_emacs_view is a weak symbol in libemacs overridden by HyaloKit @_cdecl

#endif
