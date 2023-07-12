#!/bin/bash
for subjName in sub-{009..038} ; do
    for ses in 01 02 ; do
	cd  /winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/BeLong_dataset/JLF_labels/BeLong_JLF_T1w
	target="/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/BeLong_dataset/BeLong_orig_T1w_and_T2w/${subjName}_ses-${ses}_run-1_T1w_cropped.nii.gz"
	atlases_labels_path="/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/BeLong_dataset/final_manual_labels_20230624_do-not-edit/T1w_Xavier_Fernanda_Tom/"

	command="antsJointLabelFusion.sh -d 3 -t ${target} -o ${subjName}_ses-${ses}_malf -c 2 -j 12 -x 'or' -y 's' "

	for i in {009..035}
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
