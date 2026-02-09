# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Arion?

Arion is a Haskell file watcher and test runner for hspec. It watches the file system for changes to Haskell source or test files and selectively runs associated hspec tests, supporting red-green-refactor workflows. Published on [Hackage](https://hackage.haskell.org/package/arion).

## Build and Test Commands

```bash
# Build
stack build          # or: cabal build
stack install        # install the arion executable

# Run tests
stack test           # or: cabal test

# Run the tool
arion [folder-to-watch] [source-folder] [test-folder]
arion                # defaults to: arion . src/ test/
```

Tests use hspec with hspec-discover for automatic test discovery (entry point: `test/Spec.hs`).

## Architecture

The project is a single executable with five modules under `src/Arion/`:

- **Main** (`src/Main.hs`) — Entry point; parses args and delegates to `Runner.run`
- **Runner** — Core orchestrator: sets up fsnotify file watching, builds the source-to-test file map, handles concurrency (MVars for locking, IORef Map for debouncing duplicate events with a 100ms delay)
- **EventProcessor** — Converts fsnotify `Event`s into `Command`s. Filters out non-Haskell files and emacs lock files (`.#*`). Determines if the changed file is source or test (via `typeOf`) and looks up associated tests
- **Types** — Core data types: `Command` (RunHaskell | Echo), `SourceFile` (path + module name + imports), `TestFile` (path + imports), `FileType` (Source | Test, determined by "Spec" in filename). Parses module names and imports from file content using regex
- **Utilities** — File discovery (`findHaskellFiles` recursively finds `.hs`/`.lhs` files), `associate` (maps source files to their test files via import analysis), `dependencies` (computes transitive dependencies between modules, handles cycles)

### Data Flow

1. On startup, Runner scans source and test directories, parsing each file into `SourceFile`/`TestFile` records
2. `Utilities.associate` builds a `Map FilePath [TestFile]` by matching module imports
3. fsnotify watches for file changes; events flow through `respondToEvent` → debounce logic → `processEvent` → `runCommand`
4. `processEvent` uses the pre-built map to find which tests to run when a source file changes; test file changes run themselves directly
5. Commands are executed via `callCommand` (shell process), serialized through an MVar lock

### Key Design Decisions

- File type (Source vs Test) is determined by whether "Spec" appears in the filename
- The `Command` `Show` instance produces the actual shell command string (`cabal exec runhaskell -- -isrc -itest <file>`)
- Transitive dependencies are tracked: if module A imports module B, changing B also runs A's tests
- The association map is built once at startup (file changes after startup don't update the map)

## Stack/Cabal Configuration

- Stack resolver: lts-5.3
- Language: Haskell2010
- GHC options: `-threaded`
- Test source dirs include both `test/` and `src/` (so test code can import source modules directly)
