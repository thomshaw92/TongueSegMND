import argparse
import nibabel as nib
import numpy as np
from scipy.ndimage import gaussian_filter

def expand_segmentation(input_nifti, output_nifti, direction, expansion_slices=2, label=255, sigma_mm=1.0):
    # Load NIfTI image
    img = nib.load(input_nifti)
    data = img.get_fdata()
    
    # Ensure integer format
    data = np.round(data).astype(int)

    # Create a binary mask for label 255
    binary_mask = (data == label)

    # Identify segmentation location
    x_idx, y_idx, z_idx = np.where(binary_mask)

    # Determine which axis to expand along
    if direction == 'x':
        min_idx, max_idx = np.min(x_idx), np.max(x_idx)
        for shift in range(1, expansion_slices + 1):
            if min_idx - shift >= 0:
                data[min_idx - shift, :, :] = np.where(binary_mask[min_idx, :, :], label, data[min_idx - shift, :, :])
            if max_idx + shift < data.shape[0]:
                data[max_idx + shift, :, :] = np.where(binary_mask[max_idx, :, :], label, data[max_idx + shift, :, :])

    elif direction == 'y':
        min_idx, max_idx = np.min(y_idx), np.max(y_idx)
        for shift in range(1, expansion_slices + 1):
            if min_idx - shift >= 0:
                data[:, min_idx - shift, :] = np.where(binary_mask[:, min_idx, :], label, data[:, min_idx - shift, :])
            if max_idx + shift < data.shape[1]:
                data[:, max_idx + shift, :] = np.where(binary_mask[:, max_idx, :], label, data[:, max_idx + shift, :])

    elif direction == 'z':
        min_idx, max_idx = np.min(z_idx), np.max(z_idx)
        for shift in range(1, expansion_slices + 1):
            if min_idx - shift >= 0:
                data[:, :, min_idx - shift] = np.where(binary_mask[:, :, min_idx], label, data[:, :, min_idx - shift])
            if max_idx + shift < data.shape[2]:
                data[:, :, max_idx + shift] = np.where(binary_mask[:, :, max_idx], label, data[:, :, max_idx + shift])

    else:
        raise ValueError("Invalid direction. Choose from 'x', 'y', or 'z'.")

    # Apply Gaussian smoothing (1mm)
    smoothed_data = gaussian_filter(data.astype(float), sigma=sigma_mm)

    # Re-binarize segmentation using a threshold
    final_data = np.where(smoothed_data > (label / 2), 1, 0).astype(np.uint8)  # Convert back to 1 instead of 255

    # Save the smoothed and expanded segmentation
    new_img = nib.Nifti1Image(final_data, img.affine, img.header)
    nib.save(new_img, output_nifti)

def main():
    parser = argparse.ArgumentParser(description="Expand and smooth a segmentation image in a specified direction (X, Y, or Z).")
    parser.add_argument("-i", "--input", required=True, help="Input NIfTI file")
    parser.add_argument("-o", "--output", required=True, help="Output NIfTI file")
    parser.add_argument("-d", "--direction", choices=["x", "y", "z"], required=True, help="Direction of expansion")
    parser.add_argument("-a", "--amount", type=int, default=2, help="Number of slices to expand (default: 2)")
    parser.add_argument("-s", "--sigma", type=float, default=1.0, help="Gaussian smoothing sigma in mm (default: 1.0)")

    args = parser.parse_args()
    expand_segmentation(args.input, args.output, args.direction, args.amount, sigma_mm=args.sigma)

if __name__ == "__main__":
    main()