#!/bin/bash
# Joint label fusion for subjects FIRST_SUB..LAST_SUB (sessions 01 and 02), using the
# labelled atlases FIRST_ATLAS..LAST_ATLAS in ATLAS_DIR. Subject numbers are zero-padded to 3.
[ $# -eq 6 ] || { echo "usage: $0 DATA_ROOT ATLAS_DIR FIRST_SUB LAST_SUB FIRST_ATLAS LAST_ATLAS" >&2; exit 1; }
data_root=$1
atlases_labels_path=$2/
for n in $(seq "$3" "$4") ; do
    subjName=$(printf 'sub-%03d' "$n")
    for ses in 01 02 ; do
	cd  "$data_root/BeLong_dataset/JLF_labels/BeLong_JLF_T1w"
	target="$data_root/BeLong_dataset/BeLong_orig_T1w_and_T2w/${subjName}_ses-${ses}_run-1_T1w_cropped.nii.gz"

	command="antsJointLabelFusion.sh -d 3 -t ${target} -o ${subjName}_ses-${ses}_malf -c 2 -j 12 -x 'or' -y 's' "

	for i in $(seq -f '%03g' "$5" "$6")
	do
	    atlas="${atlases_labels_path}sub-${i}_ses-01_acq-origT1w_T1w.nii.gz"
	    label="${atlases_labels_path}sub-${i}_ses-01_acq-origT1w_T1w_labels.nii.gz"
	    if [ -e $atlas ] && [ -e $label ]; then
		command+=" -g $atlas -l $label"
	    fi
	done

	echo $command
	$command
    done
done
