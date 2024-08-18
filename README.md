# Sanguine PJ Tools

Tools for easier editing and updating of VCV Rack's plugin.json manifests.

## Tools

### Sanguine PJ Editor

- A GUI tool for editing and creating the plugin.json manifests used by VCV Rack.

- The program tries to be as easy and self-evident as possible.

- Every available field documented in the VCV Rack manual for version 2.5.2 is available for editing.

- Every non-deprecated tag can be toggled on and off.

- Deprecated tags are automatically updated to their current form and saved with the manifest when saved in the program.

- The program tries to be as non-destructive as possible: the "Commit" button *must* be pressed before any changes are applied to the plugin manifest; this applies to individual modules as well: changes to individual modules must be committed on a per-module basis.

- Changes can be discarded by pressing the "Discard" button: when using this button, the latest changes are thrown away and the last committed plugin or module information is loaded from the manifest.

- Plugin information and modules are treated separately: changes to a module can be commited without updating and committing the plugin information, and visceversa.

- The program produces clean manifests: empty keys are removed.

### Sanguine Tag Updater

A command line tool for updating deprecated module tags to their current incarnations.

The tool overwrites the plugin.json you pass to it so, if you don't trust it or want to keep your old version... make a backup beforehand.

Usage:
```
sanguinetagupdater (manifestfilename | -h)

Parameters:

  manifestfilename    Mandatory. Manifest json file to be processed.

  -h                  Show this help screen.

Parameters are case insensitive
```

Example:

`sanguinetagupdater plugin.json`

Parses the manifest in plugin.json and updates the deprecated tags as needed.
