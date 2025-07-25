#!/usr/bin/env -S uv run
"""
Claude Code hook to ensure *.ndy files end with a newline.
Triggers on file write/update operations.
"""

import os
import sys
import json


def main() -> int:
    # Read hook event data from stdin
    try:
        event_data = json.load(sys.stdin)
    except (json.JSONDecodeError, EOFError):
        return 2

    # Check if this is a write or update operation
    tool_name = event_data.get("tool_name", "")
    if tool_name not in ["Write", "Edit", "MultiEdit"]:
        # dismiss other events
        return 0

    tool_response = event_data.get("tool_response", {})

    # Get the file path
    file_path = tool_response.get("filePath")
    if not file_path or not file_path.endswith(".ndy"):
        # dismiss non-ndy files
        return 0

    # Check if file exists and process it
    if os.path.exists(file_path):
        try:
            with open(file_path, "r+b") as f:
                # Seek to end to get file size
                f.seek(0, os.SEEK_END)
                file_size = f.tell()

                # Check if file is empty
                if file_size == 0:
                    return 0

                # Read last byte to check for newline
                f.seek(-1, os.SEEK_END)
                last_byte = f.read(1)

                if last_byte != b"\n":
                    f.write(b"\n")
                    print(f"Added newline to {file_path}", file=sys.stderr)

        except (IOError, OSError) as e:
            print(f"Error processing {file_path}: {e}", file=sys.stderr)

    return 0


if __name__ == "__main__":
    try:
        return_code = main()
    except Exception as e:
        print(f"Unexpected error: {e}", file=sys.stderr)
        return_code = 2
    sys.exit(return_code)
