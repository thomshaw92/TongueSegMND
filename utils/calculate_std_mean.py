import nibabel as nib
import numpy as np
import argparse

def calculate_mean(image_data, mask_data):
    masked_image_data = image_data[mask_data == 1]
    
    mean_intensity = np.mean(masked_image_data)
    
    return mean_intensity

def calculate_std(image_data):
    std_intensity = np.std(image_data)
    
    return std_intensity

def main():
    parser = argparse.ArgumentParser(description="Calculate mean and/or standard deviation of image intensities.")
    parser.add_argument("image", type=str, help="Path to the main image (NIfTI format)")
    parser.add_argument("--mask", type=str, help="Path to the mask image (NIfTI format) with 1s in the region of interest", required=False)
    parser.add_argument("--mean", action='store_true', help="Calculate mean using the mask")
    parser.add_argument("--std", action='store_true', help="Calculate standard deviation of the entire image")
    
    args = parser.parse_args()

    image_nii = nib.load(args.image)
    image_data = image_nii.get_fdata()

    mean_intensity = None
    std_intensity = None

    if args.mean and args.mask:
        mask_nii = nib.load(args.mask)
        mask_data = mask_nii.get_fdata()
        mean_intensity = calculate_mean(image_data, mask_data)
        print(f"Mean intensity: {mean_intensity}")

    if args.std:
        std_intensity = calculate_std(image_data)
        print(f"Standard deviation: {std_intensity}")

if __name__ == "__main__":
    main()