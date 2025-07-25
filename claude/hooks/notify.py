#!/bin/bash -c 'uv.exe run --script'
# /// script
# dependencies = [
#   "windows-toasts",
# ]
# ///
import json
import sys
from windows_toasts import WindowsToaster, Toast

line = sys.stdin.readline()
payload = json.loads(line)
if not isinstance(payload, dict) or "message" not in payload:
    # warn the user about to stderr
    print(
        "Error: Invalid input format. Expected a JSON object with 'message' keys.",
        file=sys.stderr,
    )
    sys.exit(1)

toaster = WindowsToaster(payload.get("title", "Claude Code"))
newToast = Toast(
    text_fields=[payload.get("message", "<No message provided>")],
    group=payload.get("session_id", None),
)
toaster.show_toast(newToast)
