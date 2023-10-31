
import numpy as np
import argparse
import nibabel as nib

def parse_arguments():
    parser = argparse.ArgumentParser(description='Remove a specified label from a MRI image.')
    parser.add_argument('input', type=str, help='Input Nifti image file path')
    parser.add_argument('output', type=str, help='Output Nifti image file path')
    parser.add_argument('--label', type=int, required=True, help='Label to remove.')
    args = parser.parse_args()
    return args

def main():
    args = parse_arguments()

    # Load the input Nifti image
    input_img_nii = nib.load(args.input)
    input_img_data = input_img_nii.get_fdata()

    # Remove specified label
    output_img_data = np.where(input_img_data == args.label, 0, input_img_data)

    # Save the output image
    output_img_nii = nib.Nifti1Image(output_img_data, input_img_nii.affine, input_img_nii.header)
    nib.save(output_img_nii, args.output)

if __name__ == "__main__":
    main()
