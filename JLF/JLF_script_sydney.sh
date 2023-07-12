#!/bin/bash
for subjName in sub-{027..160} ; do
    for ses in 01 ; do
	cd  /winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/Sydney_dataset/JLF_labelled_data/
	target="/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/Sydney_dataset/preproc_data/${subjName}_T1w_reg-to-belong_Warped.nii.gz"
	atlases_labels_path="/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/JLF_labels_atlases/T1w/"

	command="antsJointLabelFusion.sh -d 3 -t ${target} -o ${subjName}_malf -c 2 -j 12 -x 'or' -y 's' "

	for i in {009..139}
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
