import numpy as np
import argparse
import nibabel as nib
from scipy.ndimage import binary_erosion, generate_binary_structure

def erode_outer_edges(input_file, output_file, dry_run=False):
    img = nib.load(input_file)
    segmentation = img.get_fdata()
    all_labels_mask = segmentation > 0  # Create a mask where all labels are considered as one

    struct_elem = generate_binary_structure(3, 2)
    eroded_mask = binary_erosion(all_labels_mask, structure=struct_elem)

    # Identify the voxels that have been eroded by comparing the original and eroded masks
    erosion_boundary_mask = np.logical_and(all_labels_mask, np.logical_not(eroded_mask))

    eroded_segmentation = segmentation.copy()
    eroded_segmentation[erosion_boundary_mask] = 0

    if not dry_run:
        eroded_img = nib.Nifti1Image(eroded_segmentation, img.affine)
        nib.save(eroded_img, output_file)
        print(f"Erosion completed. Output saved to {output_file}")
    else:
        eroded_voxels_count = np.sum(erosion_boundary_mask)
        print(f"Dry run: {eroded_voxels_count} voxels would be eroded. Output would be saved to {output_file}")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Erode outer edges of labels in a NIfTI file.")
    parser.add_argument("input_file", type=str, help="Path to the input NIfTI file containing the segmentation to erode")
    parser.add_argument("output_file", type=str, help="Path to save the eroded NIfTI file")
    parser.add_argument("--dry-run", action='store_true', help="Perform a dry run to display what would be done without saving changes")
    
    args = parser.parse_args()

    erode_outer_edges(args.input_file, args.output_file, args.dry_run)
