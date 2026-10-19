import numpy as np
import argparse
import nibabel as nib

def parse_arguments():
    """Parse input arguments for the script."""
    parser = argparse.ArgumentParser(
        description='Remove a specified label from an MRI image. '
                    'This tool replaces the provided label value with 0 in the output image.'
    )
    
    parser.add_argument('input', type=str, help='Input Nifti image file path')
    
    parser.add_argument('output', type=str, help='Output Nifti image file path')
    
    parser.add_argument('--label', type=int, required=True, help='The label value to remove from the image.')
    
    args = parser.parse_args()
    return args

def main():
    """Load the image, remove the label and save the modified image."""
    args = parse_arguments()

    print(f"Loading image from {args.input}...")
    input_img_nii = nib.load(args.input)
    
    input_img_data = np.asanyarray(input_img_nii.dataobj)  # Preserve the original data type
    print(f"Image shape: {input_img_data.shape}, Data type: {input_img_data.dtype}")
    
    unique_labels = np.unique(input_img_data)
    print(f"Unique labels in the input image: {unique_labels}")
    
    print(f"Removing label: {args.label}")
    output_img_data = np.where(input_img_data == args.label, 0, input_img_data)

    unique_labels_after = np.unique(output_img_data)
    print(f"Unique labels after modification: {unique_labels_after}")
    
    output_img_nii = nib.Nifti1Image(output_img_data, input_img_nii.affine, input_img_nii.header)

    print(f"Saving modified image to {args.output}...")
    nib.save(output_img_nii, args.output)
    print("Image saved successfully.")

if __name__ == "__main__":
    main()