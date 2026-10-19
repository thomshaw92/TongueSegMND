import numpy as np
import argparse
import nibabel as nib
from scipy.ndimage import binary_erosion, generate_binary_structure

def erode_segmentation(input_file, output_file, label, erosion_size):
    img = nib.load(input_file)
    segmentation = img.get_fdata()

    mask = segmentation == label

    # Generate a spherical structuring element
    struct_elem = generate_binary_structure(3, 3)
    if erosion_size > 1:
        struct_elem = np.kron(struct_elem, np.ones((erosion_size, erosion_size, erosion_size)))

    eroded_mask = binary_erosion(mask, structure=struct_elem)

    eroded_segmentation = segmentation.copy()
    eroded_segmentation[eroded_mask == 0] = 0

    eroded_img = nib.Nifti1Image(eroded_segmentation, img.affine)
    nib.save(eroded_img, output_file)

def main():
    parser = argparse.ArgumentParser(description="Erode a specified label in a segmentation volume.")
    parser.add_argument("input_file", type=str, help="Input segmentation volume (NIfTI format)")
    parser.add_argument("output_file", type=str, help="Output eroded segmentation volume (NIfTI format)")
    parser.add_argument("label", type=int, help="Label number to erode")
    parser.add_argument("erosion_size", type=int, help="Erosion size in voxels, representing the radius of the spherical element")

    args = parser.parse_args()

    erode_segmentation(args.input_file, args.output_file, args.label, args.erosion_size)

if __name__ == "__main__":
    main()
