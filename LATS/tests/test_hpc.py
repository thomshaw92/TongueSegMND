import os
import shutil
import stat
import subprocess
from pathlib import Path

import pytest

HPC = Path(__file__).resolve().parents[1] / "hpc"
BASH = shutil.which("bash")


def _bash_ok():
    if not BASH:
        return False
    v = subprocess.run([BASH, "-c", "echo ${BASH_VERSINFO[0]} ${BASH_VERSINFO[1]}"],
                       capture_output=True, text=True).stdout.split()
    return (int(v[0]), int(v[1])) >= (4, 4)


pytestmark = pytest.mark.skipif(not _bash_ok(), reason="needs bash 4.4 or newer")

SBATCH = """#!/usr/bin/env bash
printf '%s\\n' "$@" > "$STUB_LOG"
echo 4242
"""

APPTAINER = """#!/usr/bin/env bash
printf '%s\\n' "$@" > "$STUB_LOG"
while [ $# -gt 0 ]; do
  if [ "$1" = -o ]; then out=$2; fi
  shift
done
if [ "$STUB_MODE" = ok ]; then mkdir -p "$out/stats"; echo x > "$out/stats/volumes.csv"; fi
"""


def _touch(p):
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(b"")


@pytest.fixture
def bids(tmp_path):
    b = tmp_path / "bids"
    for rel in [
        "sub-01/ses-01/anat/sub-01_ses-01_T1w.nii.gz",
        "sub-01/ses-02/anat/sub-01_ses-02_T1w.nii.gz",
        "sub-02/ses-01/anat/sub-02_ses-01_T1w.nii.gz",
        "sub-03/ses-02/anat/sub-03_ses-02_T1w.nii.gz",
        "sub-04/ses-01/anat/sub-04_ses-01_acq-a_T1w.nii.gz",
        "sub-04/ses-01/anat/sub-04_ses-01_acq-b_T1w.nii.gz",
        "sub-04/ses-02/anat/sub-04_ses-02_acq-a_T1w.nii.gz",
        "sub-05/anat/sub-05_T1w.nii.gz",
        "sub-06/ses-10/anat/sub-06_ses-10_T1w.nii.gz",
        "sub-06/ses-2/anat/sub-06_ses-2_T1w.nii.gz",
        "sub-06/ses-01/anat/sub-06_ses-01_T1w.nii.gz",
        "sub-07/ses-01/anat/sub-07_ses-01_T2w.nii.gz",
        "derivatives/lats/README",
    ]:
        _touch(b / rel)
    return b


@pytest.fixture
def env(tmp_path):
    stubs = tmp_path / "stubs"
    stubs.mkdir()
    for name, body in {"sbatch": SBATCH, "apptainer": APPTAINER}.items():
        p = stubs / name
        p.write_text(body)
        p.chmod(p.stat().st_mode | stat.S_IEXEC)
    e = dict(os.environ)
    e["PATH"] = f"{stubs}:{Path(BASH).parent}:{e['PATH']}"
    e["STUB_LOG"] = str(tmp_path / "stub.log")
    return e


def _run(script, *args, env, cwd, extra_env=None, ok=True):
    r = subprocess.run([str(HPC / script), *map(str, args)], cwd=cwd, capture_output=True,
                       text=True, env={**env, **(extra_env or {})})
    if ok:
        assert r.returncode == 0, r.stderr
    return r


def _rows(p):
    return [line.split("\t") for line in Path(p).read_text().splitlines()]


def test_lists_default(bids, env, tmp_path):
    _run("lats_make_lists.sh", "-b", bids, "-o", "lists", env=env, cwd=tmp_path)
    d = (bids / "derivatives" / "lats").resolve()
    longi = _rows(tmp_path / "lists/longitudinal.tsv")
    assert [r[0] for r in longi] == ["sub-01", "sub-06"]
    assert longi[0][1:3] == ["longitudinal", str(d / "longitudinal/sub-01")]
    assert [Path(x).name for x in longi[1][3:]] == [
        "sub-06_ses-01_T1w.nii.gz", "sub-06_ses-2_T1w.nii.gz", "sub-06_ses-10_T1w.nii.gz"]
    cross = _rows(tmp_path / "lists/cross-sectional.tsv")
    assert [r[:3] for r in cross] == [
        ["sub-02_ses-01", "cross-sectional", str(d / "cross-sectional/sub-02_ses-01")]]
    excl = (tmp_path / "lists/excluded.tsv").read_text()
    for s in ["sub-03", "sub-04\tses-01\t2 matching images", "sub-05\t-\tno ses-*", "sub-07"]:
        assert s in excl
    summary = (tmp_path / "lists/summary.txt").read_text()
    assert "longitudinal runs: 2" in summary and "cross-sectional runs (ses-01, --cross single): 1" in summary


def test_lists_options(bids, env, tmp_path):
    _run("lats_make_lists.sh", "-b", bids, "-o", "a", "--multi", "first", "--cross", "all",
         env=env, cwd=tmp_path)
    longi = _rows(tmp_path / "a/longitudinal.tsv")
    assert [r[0] for r in longi] == ["sub-01", "sub-04", "sub-06"]
    assert Path(longi[1][3]).name == "sub-04_ses-01_acq-a_T1w.nii.gz"
    assert [r[0] for r in _rows(tmp_path / "a/cross-sectional.tsv")] == [
        "sub-01_ses-01", "sub-02_ses-01", "sub-04_ses-01", "sub-06_ses-01"]

    _run("lats_make_lists.sh", "-b", bids, "-o", "b", "-p", "sub-06,01", "--max-sessions", "2",
         "--acq", "a", env=env, cwd=tmp_path)
    assert _rows(tmp_path / "b/longitudinal.tsv") == []
    _run("lats_make_lists.sh", "-b", bids, "-o", "c", "-p", "06", "--max-sessions", "2",
         env=env, cwd=tmp_path)
    assert len(_rows(tmp_path / "c/longitudinal.tsv")[0][3:]) == 2
    assert "ses-10\tbeyond --max-sessions 2" in (tmp_path / "c/excluded.tsv").read_text()

    done = bids / "derivatives/lats/longitudinal/sub-01/stats/volumes.csv"
    done.parent.mkdir(parents=True)
    done.write_text("x")
    _run("lats_make_lists.sh", "-b", bids, "-o", "d", "--skip-done", env=env, cwd=tmp_path)
    assert [r[0] for r in _rows(tmp_path / "d/longitudinal.tsv")] == ["sub-06"]

    r = _run("lats_make_lists.sh", "-b", bids, "--cross", "some", env=env, cwd=tmp_path, ok=False)
    assert r.returncode == 2 and "--cross must be" in r.stderr


def test_submit_and_task(bids, env, tmp_path):
    _run("lats_make_lists.sh", "-b", bids, "-o", "lists", env=env, cwd=tmp_path)
    sif = tmp_path / "lats.sif"
    sif.write_text("")
    lst = tmp_path / "lists/longitudinal.tsv"

    r = _run("lats_submit.sh", "-l", lst, "-i", sif, "--dry-run", env=env, cwd=tmp_path)
    assert "--array=1-2%20" in r.stdout and not (tmp_path / "lats_logs").exists()

    r = _run("lats_submit.sh", "-l", lst, "-i", sif, "-p", "cpu", "-c", "4", "-P", "5",
             env=env, cwd=tmp_path)
    assert "submitted array job 4242" in r.stdout
    args = Path(env["STUB_LOG"]).read_text().splitlines()
    for a in ["--array=1-2%5", "--cpus-per-task=4", "--partition=cpu", str(HPC / "lats_task.sh"),
              str(sif), "-M", "T1w", "--prior-probs"]:
        assert a in args
    frozen = Path(args[args.index(str(HPC / "lats_task.sh")) + 1])
    assert frozen.read_text() == lst.read_text()

    r = _run("lats_submit.sh", "-l", lst, "-i", sif, "--", "-M", "T1w", "-j", "2", env=env,
             cwd=tmp_path, ok=False)
    assert "set per run" in r.stderr

    slurm = {"SLURM_ARRAY_TASK_ID": "2", "SLURM_CPUS_PER_TASK": "4", "STUB_MODE": "fail"}
    lats = ["-M", "T1w", "--prior-refine"]
    r = _run("lats_task.sh", frozen, sif, *lats, env=env, cwd=tmp_path, extra_env=slurm, ok=False)
    assert r.returncode == 1 and "FAILED: sub-06" in r.stderr

    r = _run("lats_task.sh", frozen, sif, *lats, env=env, cwd=tmp_path,
             extra_env={**slurm, "STUB_MODE": "ok"})
    row = _rows(frozen)[1]
    call = Path(env["STUB_LOG"]).read_text().splitlines()
    binds = call[3].split(",")
    assert call[:3] == ["run", "--cleanenv", "--bind"] and binds[0] == row[2]
    assert sorted(binds[1:]) == sorted({f"{Path(x).parent}:{Path(x).parent}:ro" for x in row[3:]})
    assert call[4:] == [str(sif), "-o", row[2], "-j", "4", *lats, *row[3:]]
    assert "done:" in r.stdout

    Path(env["STUB_LOG"]).unlink()
    r = _run("lats_task.sh", frozen, sif, *lats, env=env, cwd=tmp_path, extra_env=slurm)
    assert "already done" in r.stdout and not Path(env["STUB_LOG"]).exists()
