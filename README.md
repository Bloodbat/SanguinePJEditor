# Sanguine PJ Tools

Tools for easier editing and updating of VCV Rack's plugin.json manifests.

## Tools

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
