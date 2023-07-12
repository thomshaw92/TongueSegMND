import argparse
import numpy as np
import nibabel as nib
from scipy.ndimage import binary_dilation, binary_erosion

def smooth_segmentation(input_path, output_path, iterations=1):
    # Load the image
    img = nib.load(input_path)

    # Get the data as a numpy array
    data = img.get_fdata()

    # Initialize an empty array for the smoothed segmentation
    smoothed = np.zeros_like(data)

    # Handle each label separately
    for label in np.unique(data):
        if label == 0:  # Skip the background
            continue
        # Perform binary dilation followed by binary erosion
        label_data = data == label
        for _ in range(iterations):
            label_data = binary_dilation(label_data)
        for _ in range(iterations):
            label_data = binary_erosion(label_data)
        smoothed[label_data] = label

    # Save the smoothed image back to a NIFTI file
    new_img = nib.Nifti1Image(smoothed, img.affine, img.header)
    nib.save(new_img, output_path)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Smooth the edges of a segmented MRI image.')
    parser.add_argument('input_path', type=str, help='Path to the input NIFTI file.')
    parser.add_argument('output_path', type=str, help='Path to the output NIFTI file.')
    parser.add_argument('--iterations', type=int, default=1, help='Number of dilation and erosion iterations.')
    args = parser.parse_args()

    smooth_segmentation(args.input_path, args.output_path, args.iterations)

