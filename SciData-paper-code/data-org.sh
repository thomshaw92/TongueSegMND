#!/bin/bash
# Rough script for organizing data and SNR/CNR calculations
# Crop data
# Remove label 5
# SNR and CNR calculations

data_dir="/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Projects/Tongue_seg/data/SciDataReleaseData2"
cropped_data_dir="${data_dir}/cropped"
SNR_CNR_dir="${data_dir}/snr-cnr-tongue-crop"
affine_dir="${data_dir}/cropped/affine_matrices"
remove_labels="/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Projects/Tongue_seg/code/TongueSegMND/utils/remove_label.py"
template="${SNR_CNR_dir}/EATT_Deformable_MM_10iters_template0_BeLong_template_space.nii.gz"
template_ROI="${SNR_CNR_dir}/ROI-template-space.nii.gz"
SNR_ROI="${SNR_CNR_dir}/SNR-temp-space.nii.gz"
SNR_MEAN="/Users/uqtshaw/Library/CloudStorage/OneDrive-TheUniversityofQueensland/Projects/Tongue_seg/code/TongueSegMND/utils/calculate_std_mean.py"

# Create directories if they don't exist
mkdir -p $cropped_data_dir $SNR_CNR_dir $affine_dir

# List all the files ending in w.nii.gz and register them to the template
#check if GenericAffine.mat exists
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
# print what is happening first to the terminal
    #check if the output file exists first
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
# print what is happening first to the terminal
    if [ ! -f ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz ]; then
        echo "antsApplyTransforms -d 3 -i ${SNR_ROI} -o ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz -r ${x} -t [${affine_dir}/$(basename ${x} .nii.gz)_affine_0GenericAffine.mat,1]"
        antsApplyTransforms -d 3 -i ${SNR_ROI} \
            -o ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz \
            -r ${x} \
            -t [${affine_dir}/$(basename ${x} .nii.gz)_affine_0GenericAffine.mat,1]
    fi
done

# Apply the ROI mask to the images ending in *w.nii.gz by multiplication using ImageMath
for x in ${data_dir}/*w.nii.gz; do
# print what is happening first to the terminal
    if [ ! -f ${cropped_data_dir}/$(basename ${x} .nii.gz)_cropped.nii.gz ] ; then
        echo "ImageMath 3 ${cropped_data_dir}/$(basename ${x} .nii.gz)_cropped.nii.gz m ${x} ${data_dir}/$(basename ${x} .nii.gz)_ROI.nii.gz"
        ImageMath 3 ${cropped_data_dir}/$(basename ${x} .nii.gz)_cropped.nii.gz m ${x} ${data_dir}/$(basename ${x} .nii.gz)_ROI.nii.gz
    fi
done

#do the same for the SNR ROI
for x in ${data_dir}/*w.nii.gz; do
# print what is happening first to the terminal
    if [ ! -f ${cropped_data_dir}/$(basename ${x} .nii.gz)_SNR_ROI.nii.gz ]; then
        echo "ImageMath 3 ${cropped_data_dir}/$(basename ${x} .nii.gz)_SNR_ROI.nii.gz m ${x} ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz"
        ImageMath 3 ${cropped_data_dir}/$(basename ${x} .nii.gz)_SNR_ROI.nii.gz m ${x} ${data_dir}/$(basename ${x} .nii.gz)_SNR_NOISE_ROI.nii.gz
    fi
done

# Apply the ROI mask to the images ending in *labels.nii.gz by multiplication using ImageMath
for x in ${data_dir}/*labels.nii.gz; do
    # Extract the base name without the suffix
    base_name=$(basename ${x} _labels.nii.gz)

    # Check if the corresponding ROI file exists and process it
    if [ -f ${data_dir}/${base_name}_ROI.nii.gz ]; then
        # Print the operation to the terminal
        if [ ! -f ${cropped_data_dir}/${base_name}_labels_cropped.nii.gz ]; then
            echo "ImageMath 3 ${cropped_data_dir}/${base_name}_labels_cropped.nii.gz m ${x} ${data_dir}/${base_name}_ROI.nii.gz"
            ImageMath 3 ${cropped_data_dir}/${base_name}_labels_cropped.nii.gz m ${x} ${data_dir}/${base_name}_ROI.nii.gz
        fi
    else
        echo "ROI file for ${x} not found. Skipping..."
    fi
done

# Remove label 5 from every cropped image
#for x in ${cropped_data_dir}/*labels_cropped.nii.gz; do
# print what is happening first to the terminal
#    echo "python ${remove_labels} --label 5 ${x} ${x}"
#    python ${remove_labels} --label 5 ${x} ${x}
#done

# Threshold the labels_cropped.nii.gz file to make all values greater than 1 equal to 1
for x in ${cropped_data_dir}/*labels_cropped.nii.gz; do
    # Print what is happening first to the terminal
    if [ ! -f ${x:0:-7}_binarised.nii.gz ]; then
        echo "Thresholding $(basename ${x}) to set all values > 1 to 1"
        # Apply the thresholding: anything greater than 1 becomes 1
        ImageMath 3 ${x:0:-7}_binarised.nii.gz ReplaceVoxelValue ${x} 1 5 1
    fi
done


# Calculate SNR for every cropped image using the SNR ROI mask (which is now the noise area)
for x in ${cropped_data_dir}/*_cropped.nii.gz; do
    # Print what is happening first to the terminal
    echo "Calculating SNR for $(basename ${x})"
    
    # The base name for corresponding files
    base_name=$(basename ${x} _cropped.nii.gz)
    
    # Use the Python script to calculate the standard deviation of noise (without mask) and the mean signal within the ROI
    noise_std=$(python ${SNR_MEAN} --std ${x} | grep "Standard deviation" | awk '{print $3}')
    mean_signal=$(python ${SNR_MEAN} --mean --mask ${cropped_data_dir}/${base_name}_ROI.nii.gz ${x} | grep "Mean intensity" | awk '{print $3}')
    
    echo "Mean signal: ${mean_signal}"
    echo "Std noise: ${std_noise}"
    
    # Check if values were correctly extracted
    if [[ -z "${mean_signal}" || -z "${noise_std}" ]]; then
        echo "Error: Failed to calculate SNR for $(basename ${x})."
        continue
    fi
    
    # Calculate SNR and save the result
    snr_value=$(echo "scale=2; ${mean_signal} / ${noise_std}" | bc)
    echo "SNR for $(basename ${x}): ${snr_value}" > ${SNR_CNR_dir}/${base_name}_SNR.txt
done
