# Feedstock Option C: Technical Learnings

## rsync for Build Directory Population

```bash
rsync -a --delete --exclude=.git emacs/ emacs-build-macos/
```

Options:
- `-a` archive mode (preserves permissions, timestamps, etc.)
- `--delete` remove files in destination not in source
- `--exclude=.git` skip git metadata (not needed in build dir)

## Pixi Task Dependencies

Dependencies declared with `depends-on = ["task_name"]` ensure proper execution order:

```toml
mac_patch = { depends-on = ["mac_prep"], ... }
mac_autogen = { depends-on = ["mac_patch"], ... }
mac_configure = { depends-on = ["mac_autogen"], ... }
```

## Output Markers for Task Caching

Pixi uses `outputs` to determine if a task needs to run:

```toml
mac_patch = { outputs = ["emacs-build-macos/.patched"], ... }
```

Once `.patched` exists, pixi skips re-running the patch task.

## Legacy Task Delegation

To maintain backward compatibility, legacy tasks delegate to new prefixed tasks:

```toml
patch = { depends-on = ["mac_patch"], description = "..." }
build = { depends-on = ["mac_build"], description = "..." }
```

## Task cwd vs Working Directory

- `cwd` in pixi.toml is relative to project root
- Commands run with emacs-build-*/ as working directory
- Paths to patches/icons use `../` to reference project root

## Testing Approach

1. Run prep tasks to populate build directories
2. Run patch tasks to verify patching works
3. Check .patched markers in build directories
4. Verify source emacs/ remains pristine
