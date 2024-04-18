import numpy as np
import argparse
import nibabel as nib
from scipy.ndimage import binary_erosion, generate_binary_structure
"""
Script Name: erode_outer_edges.py

Description:
This script performs erosion on the outer edges of all labeled regions within a NIfTI file,
ideal for refining segmentation boundaries in medical imaging analyses. It reduces the size
of each labeled region by removing the outermost layer of voxels.

Usage:
  python erode_outer_edges.py <input_file> <output_file> [--dry-run]

Arguments:
  input_file   - Path to the input NIfTI file containing the segmentation.
  output_file  - Path where the eroded segmentation NIfTI file should be saved.
  --dry-run    - Optional flag to perform a dry run which displays the number of voxels that
                 would be eroded without actually saving the output file.

Example:
  To erode edges and save the output:
    python erode_outer_edges.py input.nii.gz output.nii.gz

  To perform a dry run and see how many voxels would be eroded:
    python erode_outer_edges.py input.nii.gz output.nii.gz --dry-run

Requirements:
  - numpy
  - nibabel
  - scipy

This script uses binary erosion from the scipy.ndimage library to erode the mask of labeled
voxels. It checks each labeled voxel to see if it is on the boundary of a labeled region and
erodes it if it is, thus smoothing the overall segmentation.
"""

def erode_outer_edges(input_file, output_file, dry_run=False):
    img = nib.load(input_file)
    segmentation = img.get_fdata()
    all_labels_mask = segmentation > 0  # Create a mask where all labels are considered as one

    struct_elem = generate_binary_structure(3, 2)  # Define the structure for erosion
    eroded_mask = binary_erosion(all_labels_mask, structure=struct_elem)  # Erode the mask

    # Identify the voxels that have been eroded by comparing the original and eroded masks
    erosion_boundary_mask = np.logical_and(all_labels_mask, np.logical_not(eroded_mask))

    # Prepare the final segmentation by setting the eroded boundary voxels to 0
    eroded_segmentation = segmentation.copy()
    eroded_segmentation[erosion_boundary_mask] = 0  # Set the boundary voxels to zero

    if not dry_run:
        # Save the eroded segmentation if not in dry run mode
        eroded_img = nib.Nifti1Image(eroded_segmentation, img.affine)
        nib.save(eroded_img, output_file)
        print(f"Erosion completed. Output saved to {output_file}")
    else:
        # Dry run mode: print information without saving
        eroded_voxels_count = np.sum(erosion_boundary_mask)
        print(f"Dry run: {eroded_voxels_count} voxels would be eroded. Output would be saved to {output_file}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Erode outer edges of labels in a NIfTI file.")
    parser.add_argument("input_file", type=str, help="Path to the input NIfTI file containing the segmentation to erode")
    parser.add_argument("output_file", type=str, help="Path to save the eroded NIfTI file")
    parser.add_argument("--dry-run", action='store_true', help="Perform a dry run to display what would be done without saving changes")
    
    args = parser.parse_args()

    # Call the erosion function with the provided arguments
    erode_outer_edges(args.input_file, args.output_file, args.dry_run)
