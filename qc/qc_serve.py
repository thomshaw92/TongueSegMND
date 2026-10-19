#!/usr/bin/env python3
"""Local rating server for a Tongue-QC build directory."""

from __future__ import annotations

import argparse
import http.server
import json
import os
import shutil
import socket
import sys
import threading
import time
import webbrowser
from functools import partial
from pathlib import Path
from typing import Any, Final

APP_HTML: Final[str] = "tongue_qc.html"
DEFAULT_PORT: Final[int] = 8765
DEFAULT_CSV: Final[str] = "tongue_qc_ratings.csv"
MAX_BODY_BYTES: Final[int] = 64 * 1024 * 1024
PORT_SCAN_TRIES: Final[int] = 10

#: Explicit content types; never rely on the platform mime database.
CONTENT_TYPES: Final[dict[str, str]] = {
    ".html": "text/html; charset=utf-8",
    ".htm": "text/html; charset=utf-8",
    ".js": "text/javascript; charset=utf-8",
    ".mjs": "text/javascript; charset=utf-8",
    ".css": "text/css; charset=utf-8",
    ".json": "application/json; charset=utf-8",
    ".csv": "text/csv; charset=utf-8",
    ".txt": "text/plain; charset=utf-8",
    ".md": "text/plain; charset=utf-8",
    ".jpg": "image/jpeg",
    ".jpeg": "image/jpeg",
    ".png": "image/png",
    ".gif": "image/gif",
    ".svg": "image/svg+xml",
    ".webp": "image/webp",
    ".ico": "image/vnd.microsoft.icon",
    ".zip": "application/zip",
}


def _log(msg: str) -> None:
    """Timestamped line on stdout, flushed so it is useful under a pipe."""
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


class RatingsStore:
    """Thread-safe, crash-safe holder for the ratings CSV on disk."""

    def __init__(self, path: Path) -> None:
        self._path = path
        self._tmp = path.with_name(path.name + ".tmp")
        self._bak = path.with_name(path.name + ".bak")
        self._lock = threading.Lock()
        self._backed_up = False
        self._saves = 0

    @property
    def path(self) -> Path:
        return self._path

    @property
    def saves(self) -> int:
        return self._saves

    def load(self) -> bytes | None:
        """Current CSV bytes, or ``None`` if the file does not exist."""
        with self._lock:
            try:
                return self._path.read_bytes()
            except FileNotFoundError:
                return None
            except OSError as exc:  # unreadable / racing deletion
                _log(f"WARN  could not read {self._path.name}: {exc}")
                return None

    def save(self, text: str) -> int:
        """Write *text* atomically, returning the number of bytes written."""
        if not text.endswith("\n"):
            text += "\n"
        payload = text.encode("utf-8")
        with self._lock:
            if not self._backed_up:
                self._backed_up = True
                if self._path.exists():
                    try:
                        shutil.copy2(self._path, self._bak)
                        _log(f"backed up existing CSV -> {self._bak.name}")
                    except OSError as exc:
                        _log(f"WARN  could not write {self._bak.name}: {exc}")
            self._path.parent.mkdir(parents=True, exist_ok=True)
            with open(self._tmp, "wb") as fh:
                fh.write(payload)
                fh.flush()
                os.fsync(fh.fileno())
            os.replace(self._tmp, self._path)
            _fsync_dir(self._path.parent)
            self._saves += 1
        return len(payload)


def _fsync_dir(directory: Path) -> None:
    """Best-effort directory fsync so the rename itself is durable."""
    try:
        fd = os.open(directory, os.O_RDONLY)
    except OSError:
        return
    try:
        os.fsync(fd)
    except OSError:
        pass
    finally:
        os.close(fd)


class QCRequestHandler(http.server.SimpleHTTPRequestHandler):
    """Static file server for the build dir plus the two ``/api`` endpoints."""

    protocol_version = "HTTP/1.1"
    server_version = "TongueQC"
    sys_version = ""

    def __init__(self, *args: Any, store: RatingsStore, **kwargs: Any) -> None:
        self._store = store
        super().__init__(*args, **kwargs)

    # -- plumbing ---------------------------------------------------------
    def guess_type(self, path: str | os.PathLike[str]) -> str:
        return CONTENT_TYPES.get(Path(path).suffix.lower(), "application/octet-stream")

    def version_string(self) -> str:
        return self.server_version

    def log_message(self, fmt: str, *args: Any) -> None:
        """Quiet by default: static 2xx/3xx traffic is noise, errors are not."""
        line = fmt % args
        if " 200 " in line or " 304 " in line:
            return
        _log(line)

    def log_error(self, fmt: str, *args: Any) -> None:
        _log("ERROR " + (fmt % args))

    def _send_bytes(self, code: int, ctype: str, payload: bytes) -> None:
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(payload)))
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        if self.command != "HEAD":
            self.wfile.write(payload)

    def _send_json(self, code: int, obj: dict[str, Any]) -> None:
        self._send_bytes(code, CONTENT_TYPES[".json"], json.dumps(obj).encode("utf-8"))

    # -- endpoints --------------------------------------------------------
    def do_GET(self) -> None:  # noqa: N802  (stdlib naming)
        try:
            if self.path.split("?", 1)[0] == "/api/load":
                self._api_load()
                return
            if self.path.split("?", 1)[0].startswith("/api/"):
                self._send_json(404, {"ok": False, "error": "unknown endpoint"})
                return
            super().do_GET()
        except (BrokenPipeError, ConnectionResetError):
            pass  # the browser navigated away mid-transfer
        except Exception as exc:  # never let one bad request kill the server
            _log(f"ERROR GET {self.path}: {exc!r}")
            self._safe_error(500, "internal error")

    def do_HEAD(self) -> None:  # noqa: N802
        try:
            if self.path.split("?", 1)[0].startswith("/api/"):
                self._send_json(405, {"ok": False, "error": "HEAD not supported"})
                return
            super().do_HEAD()
        except (BrokenPipeError, ConnectionResetError):
            pass
        except Exception as exc:
            _log(f"ERROR HEAD {self.path}: {exc!r}")
            self._safe_error(500, "internal error")

    def do_POST(self) -> None:  # noqa: N802
        try:
            if self.path.split("?", 1)[0] == "/api/save":
                self._api_save()
                return
            self._send_json(404, {"ok": False, "error": "unknown endpoint"})
        except (BrokenPipeError, ConnectionResetError):
            pass
        except Exception as exc:
            _log(f"ERROR POST {self.path}: {exc!r}")
            self._safe_error(500, "internal error")

    def _api_load(self) -> None:
        data = self._store.load()
        if data is None:
            self._send_json(404, {"ok": False, "error": "no ratings CSV yet"})
            return
        self._send_bytes(200, CONTENT_TYPES[".csv"], data)

    def _api_save(self) -> None:
        raw = self._read_body()
        if raw is None:
            return  # _read_body already answered with a 400
        try:
            text = raw.decode("utf-8")
        except UnicodeDecodeError:
            self._bad_request("body is not valid UTF-8")
            return
        if not text.strip():
            # An empty body would silently erase a session's work.
            self._bad_request("refusing to write an empty CSV")
            return
        try:
            n = self._store.save(text)
        except OSError as exc:
            _log(f"ERROR could not write {self._store.path}: {exc}")
            self._send_json(500, {"ok": False, "error": str(exc)})
            return
        rows = max(text.count("\n") - 1, 0)  # minus the header
        _log(f"saved {n} bytes / {rows} row{'' if rows == 1 else 's'} "
             f"-> {self._store.path.name} (save #{self._store.saves})")
        self._send_json(200, {"ok": True, "bytes": n})

    def _read_body(self) -> bytes | None:
        """Read exactly ``Content-Length`` bytes, or answer 400 and return None."""
        raw_len = self.headers.get("Content-Length")
        if raw_len is None:
            self._bad_request("missing Content-Length")
            return None
        try:
            length = int(raw_len)
        except ValueError:
            self._bad_request("malformed Content-Length")
            return None
        if length < 0 or length > MAX_BODY_BYTES:
            self._bad_request(f"body length out of range: {length}")
            return None
        try:
            body = self.rfile.read(length)
        except (OSError, ValueError) as exc:
            self._bad_request(f"could not read body: {exc}")
            return None
        if len(body) != length:
            self._bad_request("truncated body")
            return None
        return body

    def _bad_request(self, why: str) -> None:
        _log(f"WARN  400 {self.command} {self.path}: {why}")
        self._send_json(400, {"ok": False, "error": why})

    def _safe_error(self, code: int, why: str) -> None:
        """Send an error only if the response has not already begun."""
        try:
            self._send_json(code, {"ok": False, "error": why})
        except Exception:
            pass


def _bind(handler: Any, host: str, port: int) -> tuple[http.server.ThreadingHTTPServer, int]:
    """Bind on the first free port in ``[port, port + PORT_SCAN_TRIES)``."""
    last: OSError | None = None
    for candidate in range(port, port + PORT_SCAN_TRIES):
        try:
            httpd = http.server.ThreadingHTTPServer((host, candidate), handler)
        except OSError as exc:
            if exc.errno not in (48, 98, 10048):  # EADDRINUSE across platforms
                raise
            last = exc
            continue
        httpd.daemon_threads = True
        return httpd, candidate
    raise SystemExit(
        f"could not bind {host}:{port}-{port + PORT_SCAN_TRIES - 1} ({last}). "
        "Another qc_serve.py is probably already running -- use --port."
    )


def _banner(url: str, directory: Path, csv_path: Path) -> None:
    bar = "=" * 68
    print(f"\n{bar}")
    print("  Tongue-QC rating server")
    print(bar)
    print(f"  Open:      {url}")
    print(f"  Serving:   {directory}")
    print(f"  Ratings:   {csv_path}   (atomic writes, .bak on first overwrite)")
    if not (directory / APP_HTML).exists():
        print(f"  WARNING:   {APP_HTML} not found here -- is this a build dir?")
    print(f"{bar}\n  Ctrl-C to stop. Ratings are checkpointed after every scan.\n{bar}\n",
          flush=True)


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        prog="qc_serve.py",
        description="Serve a Tongue-QC build and checkpoint ratings to disk.",
    )
    p.add_argument("--dir", type=Path, default=Path("."),
                   help="build directory to serve (default: the current directory)")
    p.add_argument("--port", type=int, default=DEFAULT_PORT,
                   help=f"port on 127.0.0.1 (default: {DEFAULT_PORT})")
    p.add_argument("--csv", type=Path, default=Path(DEFAULT_CSV),
                   help=f"ratings CSV, relative to --dir unless absolute "
                        f"(default: {DEFAULT_CSV})")
    p.add_argument("--no-open", action="store_true",
                   help="do not open a browser window")
    return p.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    directory = args.dir.expanduser().resolve()
    if not directory.is_dir():
        raise SystemExit(f"not a directory: {directory}")
    csv_arg = args.csv.expanduser()
    csv_path = (csv_arg if csv_arg.is_absolute() else directory / csv_arg).resolve()

    store = RatingsStore(csv_path)
    handler = partial(QCRequestHandler, store=store, directory=str(directory))
    host = "127.0.0.1"  # loopback only
    httpd, port = _bind(handler, host, args.port)

    url = f"http://{host}:{port}/{APP_HTML}"
    _banner(url, directory, csv_path)

    if not args.no_open:
        # Defer slightly so the banner is on screen before the browser steals focus.
        threading.Timer(0.4, webbrowser.open, args=(url,)).start()

    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print()
        _log(f"stopping ({store.saves} checkpoint(s) written)")
    finally:
        httpd.shutdown()
        httpd.server_close()
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except socket.error as exc:  # pragma: no cover - environment dependent
        raise SystemExit(f"network error: {exc}") from exc
