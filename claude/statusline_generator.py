#!/usr/bin/env python3
"""
Claude Code Status Line Generator

This script generates a custom status line for Claude Code by processing JSON input
from stdin and formatting it with git branch information, current directory, model name,
and session ID.
"""

import json
import os
import subprocess
import sys
from pathlib import Path

# ANSI color codes
COLORS = {
    "green": "\033[01;32m",
    "blue": "\033[01;34m",
    "red": "\033[01;31m",
    "yellow": "\033[01;33m",
    "magenta": "\033[01;35m",
    "cyan": "\033[01;36m",
    "reset": "\033[00m",
}


def read_input():
    """Read and parse JSON input from stdin."""
    try:
        return json.load(sys.stdin)
    except json.JSONDecodeError as e:
        print(f"Error parsing JSON input: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"Error reading input: {e}", file=sys.stderr)
        sys.exit(1)


def get_debian_chroot_prefix():
    """Get debian chroot prefix if available."""
    try:
        chroot_file = Path("/etc/debian_chroot")
        if chroot_file.exists():
            debian_chroot = chroot_file.read_text().strip()
            if debian_chroot:
                return f"({debian_chroot})"
    except Exception:
        pass
    return ""


def get_git_branch(directory):
    """Get current git branch for the given directory."""
    try:
        original_dir = os.getcwd()
        os.chdir(directory)

        # Check if we're in a git repository
        result = subprocess.run(
            ["git", "rev-parse", "--git-dir"], capture_output=True, text=True
        )

        if result.returncode != 0:
            return ""

        # Get current branch
        result = subprocess.run(
            ["git", "branch", "--show-current"], capture_output=True, text=True
        )

        branch_name = (
            result.stdout.strip() if result.returncode == 0 else "detached"
        )
        if not branch_name:
            branch_name = "detached"

        return f"{COLORS['magenta']} {branch_name}{COLORS['reset']}"

    except Exception:
        return ""
    finally:
        try:
            os.chdir(original_dir)
        except Exception:
            pass


def get_display_path(current_dir):
    """Convert absolute path to display path (with ~ for home)."""
    home = os.path.expanduser("~")
    if current_dir.startswith(home):
        return current_dir.replace(home, "~", 1)
    return current_dir


def format_context_usage(context_window):
    """Format context window usage as a percentage."""
    if not context_window:
        return ""

    context_size = context_window.get("context_window_size", 0)
    current_usage = context_window.get("current_usage")

    if not context_size or not current_usage:
        return ""

    # Calculate current context tokens
    input_tokens = current_usage.get("input_tokens", 0)
    cache_creation = current_usage.get("cache_creation_input_tokens", 0)
    cache_read = current_usage.get("cache_read_input_tokens", 0)

    # Include autocompact buffer (22.5%) as already used
    autocompact_buffer = int(context_size * 0.225)
    current_tokens = input_tokens + cache_creation + cache_read
    percent_used = ((current_tokens + autocompact_buffer) * 100) // context_size

    return f"{percent_used}%"


def generate_status_line(input_data):
    """Generate the complete status line."""
    if not input_data:
        return ""

    # Extract data from input
    current_dir = input_data.get("workspace", {}).get("current_dir", "/")
    model_name = input_data.get("model", {}).get("display_name", "Unknown")
    context_window = input_data.get("context_window", {})

    # Ensure we can access the directory
    if not os.path.exists(current_dir):
        current_dir = "/"

    # Get components
    chroot_prefix = get_debian_chroot_prefix()
    display_path = get_display_path(current_dir)
    git_branch = get_git_branch(current_dir)
    context_usage = format_context_usage(context_window)

    # Format the status line
    status_line = (
        f"{chroot_prefix}"
        f"📁 {COLORS['cyan']}{display_path}{COLORS['reset']} "
        f"{git_branch} "
        f"🤖 {COLORS['green']}{model_name}{COLORS['reset']}"
    )

    # Add context usage if available
    if context_usage:
        status_line += (
            f" 📊 {COLORS['yellow']}{context_usage}{COLORS['reset']}"
        )

    return status_line


def main():
    """Main entry point."""
    input_data = read_input()
    status_line = generate_status_line(input_data)
    print(status_line)


if __name__ == "__main__":
    main()
