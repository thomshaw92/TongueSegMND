#!/bin/bash
# Crop data to the Template defined ROI
# SNR and CNR calculations

data_dir="${TONGUE_ROOT:-.}/Projects/Tongue_seg/data/SciDataReleaseData"
cropped_data_dir="${data_dir}/cropped"
SNR_CNR_dir="${data_dir}/snr-cnr-tongue-crop"
affine_dir="${data_dir}/cropped/affine_matrices"
remove_labels="$(dirname "$0")/../utils/remove_label.py"
template="${SNR_CNR_dir}/EATT_Deformable_MM_10iters_template0_BeLong_template_space.nii.gz"
template_ROI="${SNR_CNR_dir}/ROI-template-space.nii.gz"
SNR_ROI="${SNR_CNR_dir}/SNR-temp-space.nii.gz"
SNR_MEAN="$(dirname "$0")/../utils/calculate_std_mean.py"

# Create directories if they don't exist
mkdir -p $cropped_data_dir $SNR_CNR_dir $affine_dir

# Register the *w.nii.gz images to the template
for x in ${data_dir}/*w.nii.gz; do
if [ ! -f ${affine_dir}/$(basename ${x} .nii.gz)_affine_0GenericAffine.mat ]; then
    antsRegistrationSyNQuick.sh -d 3 -m ${x} \
        -f ${template} \
        -t r \
        -o ${affine_dir}/$(basename ${x} .nii.gz)_affine_
fi
done

# Apply the transform to the ROI
for x in ${data_dir}/*w.nii.gz; do
    if [ ! -f ${data_dir}/$(basename ${x} .nii.gz)_ROI.nii.gz ]; then
        echo "antsApplyTransforms -d 3 -i ${template_ROI} -o ${data_dir}/$(basename ${x} .nii.gz)_ROI.nii.gz -r ${x} -t [${affine_dir}/$(basename ${x} .nii.gz)_affine_0GenericAffine.mat,1]"
        antsApplyTransforms -d 3 -i ${template_ROI} \
            -o ${data_dir}/$(basename ${x} .nii.gz)_ROI.nii.gz \
            -r ${x} \
            -t [${affine_dir}/$(basename ${x} .nii.gz)_affine_0GenericAffine.mat,1]
    fi
done

# Apply the transform to the SNR ROI
for x in ${data_dir}/*w.nii.gz; do
    if [ ! -f ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz ]; then
        echo "antsApplyTransforms -d 3 -i ${SNR_ROI} -o ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz -r ${x} -t [${affine_dir}/$(basename ${x} .nii.gz)_affine_0GenericAffine.mat,1]"
        antsApplyTransforms -d 3 -i ${SNR_ROI} \
            -o ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz \
            -r ${x} \
            -t [${affine_dir}/$(basename ${x} .nii.gz)_affine_0GenericAffine.mat,1]
    fi
done

# Apply the ROI mask to the *w.nii.gz images
for x in ${data_dir}/*w.nii.gz; do
    if [ ! -f ${cropped_data_dir}/$(basename ${x} .nii.gz)_cropped.nii.gz ] ; then
        echo "ImageMath 3 ${cropped_data_dir}/$(basename ${x} .nii.gz)_cropped.nii.gz m ${x} ${data_dir}/$(basename ${x} .nii.gz)_ROI.nii.gz"
        ImageMath 3 ${cropped_data_dir}/$(basename ${x} .nii.gz)_cropped.nii.gz m ${x} ${data_dir}/$(basename ${x} .nii.gz)_ROI.nii.gz
    fi
done

#do the same for the SNR ROI
for x in ${data_dir}/*w.nii.gz; do
    if [ ! -f ${cropped_data_dir}/$(basename ${x} .nii.gz)_SNR_ROI.nii.gz ]; then
        echo "ImageMath 3 ${cropped_data_dir}/$(basename ${x} .nii.gz)_SNR_ROI.nii.gz m ${x} ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz"
        ImageMath 3 ${cropped_data_dir}/$(basename ${x} .nii.gz)_SNR_ROI.nii.gz m ${x} ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz
    fi
done

# Apply the ROI mask to the *labels.nii.gz images
for x in ${data_dir}/*labels.nii.gz; do
    # Extract the base name without the suffix
    base_name=$(basename ${x} _labels.nii.gz)

    # Check if the corresponding ROI file exists and process it
    if [ -f ${data_dir}/${base_name}_ROI.nii.gz ]; then
        if [ ! -f ${cropped_data_dir}/${base_name}_labels_cropped.nii.gz ]; then
        echo "ImageMath 3 ${cropped_data_dir}/${base_name}_labels_cropped.nii.gz m ${x} ${data_dir}/${base_name}_ROI.nii.gz"
        ImageMath 3 ${cropped_data_dir}/${base_name}_labels_cropped.nii.gz m ${x} ${data_dir}/${base_name}_ROI.nii.gz
        fi
    else
        echo "ROI file for ${x} not found. Skipping..."
    fi
done


# Threshold the cropped labels: values greater than 1 become 1
for x in ${cropped_data_dir}/*labels_cropped.nii.gz; do
    if [ ! -f ${x:0:-7}_binarised.nii.gz ]; then
        echo "Thresholding $(basename ${x}) to set all values > 1 to 1"
        ImageMath 3 ${x:0:-7}_binarised.nii.gz ReplaceVoxelValue ${x} 1 5 1
    fi
done

#make all the greyscale images between 0 and 1
for x in ${cropped_data_dir}/*w_cropped.nii.gz; do
    if [ ! -f ${x:0:-7}_normalised.nii.gz ]; then
        ImageMath 3 ${x:0:-7}_normalised.nii.gz RescaleImage ${x} 0 1
    fi
done

#do the same for the SNR ROI 
for x in ${cropped_data_dir}/*_SNR_ROI.nii.gz; do
    if [ ! -f ${x:0:-7}_normalised.nii.gz ]; then
        ImageMath 3 ${x:0:-7}_normalised.nii.gz RescaleImage ${x} 0 1
    fi
done

# Calculate SNR for every cropped image
for x in ${cropped_data_dir}/*w_cropped_normalised.nii.gz; do
    # Extract the segID component from the filename
    segID=$(basename ${x} | grep -o 'segID-[0-9]\{3\}')
    
    echo "Calculating SNR for $(basename ${x})" 
    
    # Use the Python script to calculate the mean signal within the ROI
    mean_signal=$(python ${SNR_MEAN} --mean --mask ${cropped_data_dir}/*${segID}*_labels_cropped_binarised.nii.gz ${x} | grep "Mean intensity" | awk '{print $3}')
    
    echo ${mean_signal} > ${SNR_CNR_dir}/${segID}_mean_signal.txt
done

# Calculate the standard deviation of noise using the SNR ROI
for x in ${cropped_data_dir}/*_SNR_ROI_normalised.nii.gz; do
    # Extract the segID component from the filename
    segID=$(basename ${x} | grep -o 'segID-[0-9]\{3\}')
    
    echo "Calculating noise standard deviation for $(basename ${x})"
    
    # Use the Python script to calculate the standard deviation of noise (without mask)
    std_noise=$(python ${SNR_MEAN} --std ${x} | grep "Standard deviation" | awk '{print $3}')
    
    echo ${std_noise} > ${SNR_CNR_dir}/${segID}_std_noise.txt
done


#calculate the mean signal of tissue 1 in the labels_cropped file
for x in ${cropped_data_dir}/*labels_cropped.nii.gz; do
        echo "Thresholding $(basename ${x}) to set all values > 1 to 0"
        ImageMath 3 ${x:0:-7}_label_1.nii.gz ReplaceVoxelValue ${x} 2 5 0
done
#then do the same for label 2
for x in ${cropped_data_dir}/*labels_cropped.nii.gz; do
        echo "Thresholding $(basename ${x}) to set value 2 to 1"
        ImageMath 3 ${x:0:-7}_intermediate.nii.gz ReplaceVoxelValue ${x} 0 1 0
        ImageMath 3 ${x:0:-7}_intermediate_2.nii.gz ReplaceVoxelValue ${x:0:-7}_intermediate.nii.gz 3 5 0
        ImageMath 3 ${x:0:-7}_label_2.nii.gz ReplaceVoxelValue ${x:0:-7}_intermediate_2.nii.gz 2 2 1
        rm ${x:0:-7}_intermediate.nii.gz ${x:0:-7}_intermediate_2.nii.gz
done

# Calculate the mean signal for label 1
for x in ${cropped_data_dir}/*_label_1.nii.gz; do
    # Extract the segID component from the filename
    segID=$(basename ${x} | grep -o 'segID-[0-9]\{3\}')
    
    # Use the Python script to calculate the mean signal within the ROI
    mean_signal=$(python ${SNR_MEAN} --mean --mask ${x} ${cropped_data_dir}/*${segID}*w_cropped_normalised.nii.gz | grep "Mean intensity" | awk '{print $3}')
    
    echo ${mean_signal} > ${SNR_CNR_dir}/${segID}_mean_signal_tissue_1.txt
done
#and do the same for label 2
for x in ${cropped_data_dir}/*_label_2.nii.gz; do
    # Extract the segID component from the filename
    segID=$(basename ${x} | grep -o 'segID-[0-9]\{3\}')
    # Use the Python script to calculate the mean signal within the ROI
    mean_signal=$(python ${SNR_MEAN} --mean --mask ${x} ${cropped_data_dir}/*${segID}*w_cropped_normalised.nii.gz | grep "Mean intensity" | awk '{print $3}')
    
    echo ${mean_signal} > ${SNR_CNR_dir}/${segID}_mean_signal_tissue_2.txt
done

# Calculate the SNR
for x in ${SNR_CNR_dir}/*_mean_signal.txt; do
    # Extract the segID component from the filename
    segID=$(basename ${x} | grep -o 'segID-[0-9]\{3\}')
    
    echo "Calculating final SNR for ${segID}"
    
    mean_signal=$(cat ${SNR_CNR_dir}/${segID}_mean_signal.txt)
    std_noise=$(cat ${SNR_CNR_dir}/${segID}_std_noise.txt)
    
    # Calculate SNR
    SNR=$(echo "scale=2; $mean_signal / $std_noise" | bc)
    
    echo ${SNR} > ${SNR_CNR_dir}/${segID}_SNR.txt
done


# Calculate the CNR
for x in ${SNR_CNR_dir}/*_mean_signal_tissue_1.txt; do
    # Extract the segID component from the filename
    segID=$(basename ${x} | grep -o 'segID-[0-9]\{3\}')
    
    echo "Calculating final CNR for ${segID}"
    
    mean_signal_tissue_1=$(cat ${SNR_CNR_dir}/${segID}_mean_signal_tissue_1.txt)
    mean_signal_tissue_2=$(cat ${SNR_CNR_dir}/${segID}_mean_signal_tissue_2.txt)
    std_noise=$(cat ${SNR_CNR_dir}/${segID}_std_noise.txt)
    
    # Calculate CNR
    CNR=$(echo "scale=2; ($mean_signal_tissue_1 - $mean_signal_tissue_2) / $std_noise" | bc)
    
    echo ${CNR} > ${SNR_CNR_dir}/${segID}_CNR.txt
done


