#!/bin/bash
ml ants/2.3.4

antsJointLabelFusion.sh -d 3 -t EATT_template0.nii.gz -o malf -c 2 -j 4 \                       
    -p malfPosteriors%04d.nii.gz \                                                                                                
    -g sub-000_ses-01_Sydney_segID-111_tongueID-080_T1w.nii.gz -l sub-000_ses-01_Sydney_segID-111_tongueID-080_T1w_labels.nii.gz \
    -g sub-001_ses-01_Sydney_segID-112_tongueID-081_T1w.nii.gz -l sub-001_ses-01_Sydney_segID-112_tongueID-081_T1w_labels.nii.gz \
    -g sub-013_ses-01_Sydney_segID-122_tongueID-082_T1w.nii.gz -l sub-013_ses-01_Sydney_segID-122_tongueID-082_T1w_labels.nii.gz \
    -g sub-015_ses-01_Sydney_segID-124_tongueID-083_T1w.nii.gz -l sub-015_ses-01_Sydney_segID-124_tongueID-083_T1w_labels.nii.gz \
    -g sub-027_ses-01_Sydney_segID-131_tongueID-084_T1w.nii.gz -l sub-027_ses-01_Sydney_segID-131_tongueID-084_T1w_labels.nii.gz \
    -g sub-035_ses-01_Sydney_segID-134_tongueID-085_T1w.nii.gz -l sub-035_ses-01_Sydney_segID-134_tongueID-085_T1w_labels.nii.gz \
    -g sub-037_ses-01_Sydney_segID-136_tongueID-086_T1w.nii.gz -l sub-037_ses-01_Sydney_segID-136_tongueID-086_T1w_labels.nii.gz \
    -g sub-078_ses-01_Sydney_segID-162_tongueID-087_T1w.nii.gz -l sub-078_ses-01_Sydney_segID-162_tongueID-087_T1w_labels.nii.gz \
    -g sub-081_ses-01_Sydney_segID-163_tongueID-088_T1w.nii.gz -l sub-081_ses-01_Sydney_segID-163_tongueID-088_T1w_labels.nii.gz \
    -g sub-082_ses-01_Sydney_segID-164_tongueID-089_T1w.nii.gz -l sub-082_ses-01_Sydney_segID-164_tongueID-089_T1w_labels.nii.gz \
    -g sub-084_ses-01_Sydney_segID-165_tongueID-091_T1w.nii.gz -l sub-084_ses-01_Sydney_segID-165_tongueID-091_T1w_labels.nii.gz \
    -g sub-085_ses-01_Sydney_segID-166_tongueID-092_T1w.nii.gz -l sub-085_ses-01_Sydney_segID-166_tongueID-092_T1w_labels.nii.gz \
    -g sub-087_ses-01_Sydney_segID-167_tongueID-093_T1w.nii.gz -l sub-087_ses-01_Sydney_segID-167_tongueID-093_T1w_labels.nii.gz \
    -g sub-089_ses-01_Sydney_segID-169_tongueID-094_T1w.nii.gz -l sub-089_ses-01_Sydney_segID-169_tongueID-094_T1w_labels.nii.gz \
    -g sub-090_ses-01_Sydney_segID-170_tongueID-095_T1w.nii.gz -l sub-090_ses-01_Sydney_segID-170_tongueID-095_T1w_labels.nii.gz \
    -g sub-095_ses-01_Sydney_segID-174_tongueID-096_T1w.nii.gz -l sub-095_ses-01_Sydney_segID-174_tongueID-096_T1w_labels.nii.gz \
    -g sub-096_ses-01_Sydney_segID-175_tongueID-097_T1w.nii.gz -l sub-096_ses-01_Sydney_segID-175_tongueID-097_T1w_labels.nii.gz \
    -g sub-098_ses-01_Sydney_segID-177_tongueID-098_T1w.nii.gz -l sub-098_ses-01_Sydney_segID-177_tongueID-098_T1w_labels.nii.gz \
    -g sub-099_ses-01_Sydney_segID-178_tongueID-099_T1w.nii.gz -l sub-099_ses-01_Sydney_segID-178_tongueID-099_T1w_labels.nii.gz \
    -g sub-107_ses-01_Sydney_segID-183_tongueID-100_T1w.nii.gz -l sub-107_ses-01_Sydney_segID-183_tongueID-100_T1w_labels.nii.gz \
    -g sub-108_ses-01_Sydney_segID-184_tongueID-101_T1w.nii.gz -l sub-108_ses-01_Sydney_segID-184_tongueID-101_T1w_labels.nii.gz \
    -g sub-130_ses-01_Sydney_segID-190_tongueID-102_T1w.nii.gz -l sub-130_ses-01_Sydney_segID-190_tongueID-102_T1w_labels.nii.gz \
    -g sub-139_ses-01_Sydney_segID-194_tongueID-103_T1w.nii.gz -l sub-139_ses-01_Sydney_segID-194_tongueID-103_T1w_labels.nii.gz \
    -g sub-140_ses-01_Sydney_segID-195_tongueID-104_T1w.nii.gz -l sub-140_ses-01_Sydney_segID-195_tongueID-104_T1w_labels.nii.gz