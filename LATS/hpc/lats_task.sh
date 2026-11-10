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

if [ -s "$out/stats/volumes.csv" ]; then
  echo "already done, skipping: $out/stats/volumes.csv exists"
  exit 0
fi
for img in "${imgs[@]}"; do
  [ -r "$img" ] || { echo "cannot read image: $img" >&2; exit 1; }
done
command -v apptainer >/dev/null || { echo "apptainer not found on PATH" >&2; exit 1; }
mkdir -p "$out"

binds=$out
while read -r dir; do binds+=",$dir:$dir:ro"; done < <(for img in "${imgs[@]}"; do dirname "$img"; done | sort -u)

export APPTAINERENV_OMP_NUM_THREADS=$cpus
export APPTAINERENV_MKL_NUM_THREADS=$cpus
export APPTAINERENV_OPENBLAS_NUM_THREADS=$cpus
export APPTAINERENV_NIPYPE_NO_ET=1

cmd=(apptainer run --cleanenv --bind "$binds" "$sif" -o "$out" -j "$cpus" "$@" "${imgs[@]}")
printf 'command:'
printf ' %q' "${cmd[@]}"
echo

rc=0
"${cmd[@]}" || rc=$?
echo "end: $(date '+%Y-%m-%d %H:%M:%S'), lats exit code $rc"
if [ "$rc" -ne 0 ] || [ ! -s "$out/stats/volumes.csv" ]; then
  echo "FAILED: $id (lats exit code $rc; stats/volumes.csv present: $([ -s "$out/stats/volumes.csv" ] && echo yes || echo no))" >&2
  exit 1
fi
echo "done: $out/stats/volumes.csv"
