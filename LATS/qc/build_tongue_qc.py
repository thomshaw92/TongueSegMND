#!/usr/bin/env python3
"""Command-line entry point for the Tongue-QC builder (see tongue_qc.build)."""

from __future__ import annotations

from tongue_qc.build import main

if __name__ == "__main__":
    raise SystemExit(main())
