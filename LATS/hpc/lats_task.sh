#!/usr/bin/env bash
set -euo pipefail

if [ $# -lt 3 ] || [ -z "${SLURM_ARRAY_TASK_ID:-}" ]; then
  echo "Usage: lats_task.sh LIST IMAGE.sif LATS_OPTIONS..." >&2
  echo "Run by lats_submit.sh as a Slurm array task; line SLURM_ARRAY_TASK_ID of LIST is the run." >&2
  exit 2
fi

list=$1 sif=$2
shift 2
i=$SLURM_ARRAY_TASK_ID
cpus=${SLURM_CPUS_PER_TASK:-1}

line=$(sed -n "${i}p" "$list")
[ -n "$line" ] || { echo "no line $i in $list" >&2; exit 2; }
IFS=$'\t' read -r -a f <<<"$line"
id=${f[0]} kind=${f[1]} out=${f[2]}
imgs=("${f[@]:3}")

echo "run: $id ($kind, ${#imgs[@]} session(s)), line $i of $list"
echo "host: $(hostname), cpus: $cpus, start: $(date '+%Y-%m-%d %H:%M:%S')"
printf 'image: %s\n' "${imgs[@]}"
echo "output: $out"

sha256() { if command -v sha256sum >/dev/null; then sha256sum "$1"; else shasum -a 256 "$1"; fi | cut -d' ' -f1; }
qa_labels() {
  local id=$1 qd=$2 sub=${1%%_*}
  if [ "$id" = "$sub" ]; then echo "$qd/$sub/anat/${sub}_space-sst_desc-qa_dseg.nii.gz"
  else echo "$qd/$sub/${id#*_}/anat/${id}_space-sst_desc-qa_dseg.nii.gz"; fi
}
qa_included() { local j=${1%.nii.gz}.json; [ ! -f "$j" ] || { grep -q '"status": "done"' "$j" && grep -q '"decision": "include"' "$j"; }; }
qa_applied() {
  local j=${2%.nii.gz}.json
  grep -q "\"sha256\": \"$(sha256 "$2")\"" "$1/qa_applied.json" 2>/dev/null || return 1
  if [ -f "$j" ]; then grep -q "\"sidecar_sha256\": \"$(sha256 "$j")\"" "$1/qa_applied.json"
  else grep -q '"sidecar_sha256": null' "$1/qa_applied.json"; fi
}
qa_args=() want=
if [ -n "${LATS_QA_DIR:-}" ]; then
  qa_file=$(qa_labels "$id" "$LATS_QA_DIR")
  if [ ! -r "$qa_file" ] || ! qa_included "$qa_file"; then
    echo "no included QA labels for $id, nothing to do: $qa_file"
    exit 0
  fi
  want=$qa_file
  if qa_applied "$out" "$qa_file" && [ -s "$out/stats/volumes.csv" ]; then
    echo "QA labels already applied: $qa_file"
    exit 0
  fi
  qa_args=(--qa-labels "$qa_file")
  echo "qa labels: $qa_file"
elif [ -s "$out/stats/volumes.csv" ]; then
  echo "already done, skipping: $out/stats/volumes.csv exists"
  exit 0
fi
for img in "${imgs[@]}"; do
  [ -r "$img" ] || { echo "cannot read image: $img" >&2; exit 1; }
done
command -v apptainer >/dev/null || { echo "apptainer not found on PATH" >&2; exit 1; }
mkdir -p "$out"

binds=$out
while read -r dir; do binds+=",$dir:$dir:ro"; done < <(for f in "${imgs[@]}" "${qa_args[@]:1}"; do dirname "$f"; done | sort -u)

export APPTAINERENV_OMP_NUM_THREADS=$cpus
export APPTAINERENV_MKL_NUM_THREADS=$cpus
export APPTAINERENV_OPENBLAS_NUM_THREADS=$cpus
export APPTAINERENV_NIPYPE_NO_ET=1

cmd=(apptainer run --cleanenv --bind "$binds" "$sif" -o "$out" -j "$cpus" "$@" "${qa_args[@]}" "${imgs[@]}")
printf 'command:'
printf ' %q' "${cmd[@]}"
echo

rc=0
"${cmd[@]}" || rc=$?
echo "end: $(date '+%Y-%m-%d %H:%M:%S'), lats exit code $rc"
applied=yes
[ -z "$want" ] || qa_applied "$out" "$want" || applied=no
if [ "$rc" -ne 0 ] || [ ! -s "$out/stats/volumes.csv" ] || [ "$applied" = no ]; then
  echo "FAILED: $id (lats exit code $rc; stats/volumes.csv present: $([ -s "$out/stats/volumes.csv" ] && echo yes || echo no)${want:+; QA labels applied: $applied})" >&2
  exit 1
fi
echo "done: $out/stats/volumes.csv"
