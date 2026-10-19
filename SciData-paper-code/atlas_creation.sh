#!/bin/bash
ml ants/2.3.4

# atlas_list.txt (kept with the data): one "image labels" pair per line
atlas_args=()
while read -r g l; do atlas_args+=(-g "$g" -l "$l"); done < atlas_list.txt
antsJointLabelFusion.sh -d 3 -t EATT_template0.nii.gz -o malf -c 2 -j 4 \
    -p malfPosteriors%04d.nii.gz \
    "${atlas_args[@]}"