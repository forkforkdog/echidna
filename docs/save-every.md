# Periodic Coverage Saving

## Overview

Echidna now supports saving coverage data periodically during campaign execution using the `--save-every` flag. This feature prevents loss of coverage data in case of crashes or interruptions during long-running campaigns.

## Usage

### Command Line

```bash
echidna test.sol --save-every 5
```

This will save coverage data every 5 minutes during the campaign execution.

### Configuration File

You can also configure periodic saving in your `echidna.yaml`:

```yaml
saveEvery: 10  # Save coverage every 10 minutes
```

## How It Works

When `--save-every` is specified:

1. Echidna spawns a background thread that runs alongside the fuzzing campaign
2. Every N minutes (as specified), the thread:
   - Takes a thread-safe snapshot of the current coverage maps
   - Saves the coverage data to timestamped files
   - Continues without interrupting the campaign

### Output Location

Coverage snapshots are saved to:
```
<corpus-dir>/coverage-snapshots/covered.<seed>.<timestamp>.<format>
```

Where:
- `<seed>`: The campaign seed
- `<timestamp>`: Unix timestamp when the snapshot was taken
- `<format>`: The coverage format (lcov, html, or txt)

### File Formats

The periodic saves respect the same `coverageFormats` configuration as the final coverage report:
- `lcov`: LCOV format for integration with coverage tools
- `html`: HTML format for visual inspection
- `txt`: Plain text format

## Examples

### Save coverage every minute during a 10-minute campaign
```bash
echidna test.sol --save-every 1 --timeout 600
```

### Configure in echidna.yaml
```yaml
testLimit: 100000
saveEvery: 5
coverageFormats:
  - html
  - lcov
corpusDir: ./corpus
```

## Implementation Details

- Coverage saving is thread-safe and doesn't pause the campaign
- The coverage maps are read-only accessed during snapshots
- Minimal performance impact on the fuzzing campaign
- Automatically cleaned up when the campaign ends

## Use Cases

1. **Long-running campaigns**: Prevent data loss in multi-hour/day campaigns
2. **Debugging**: Track coverage progression over time
3. **CI/CD**: Generate intermediate coverage reports for monitoring
4. **Research**: Analyze coverage growth patterns during fuzzing