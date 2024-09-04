import numpy as np
import argparse
import nibabel as nib

# Function to parse the command-line arguments
def parse_arguments():
    """
    Parse input arguments for the script.
    
    This function uses argparse to gather the required input Nifti image file path,
    output file path, and the label to remove. The user must specify these through 
    the command line.
    
    Returns:
        args: Parsed command-line arguments.
    """
    parser = argparse.ArgumentParser(
        description='Remove a specified label from an MRI image. '
                    'This tool replaces the provided label value with 0 in the output image.'
    )
    
    # Input image file path (Nifti format)
    parser.add_argument('input', type=str, help='Input Nifti image file path')
    
    # Output image file path (where the modified image will be saved)
    parser.add_argument('output', type=str, help='Output Nifti image file path')
    
    # Label that needs to be removed (this will be replaced by 0 in the output image)
    parser.add_argument('--label', type=int, required=True, help='The label value to remove from the image.')
    
    # Parse the arguments
    args = parser.parse_args()
    return args

# Main function that performs the label removal operation
def main():
    """
    Main function to handle image loading, label removal, and saving the modified image.
    """
    # Parse arguments from the command line
    args = parse_arguments()

    # Load the input Nifti image
    print(f"Loading image from {args.input}...")
    input_img_nii = nib.load(args.input)
    
    # Determine if the image is in integer or float format, and handle accordingly
    input_img_data = np.asanyarray(input_img_nii.dataobj)  # Preserve the original data type
    print(f"Image shape: {input_img_data.shape}, Data type: {input_img_data.dtype}")
    
    # Display the unique labels in the image before modification (for verification)
    unique_labels = np.unique(input_img_data)
    print(f"Unique labels in the input image: {unique_labels}")
    
    # Remove the specified label by setting all voxels with that label to 0
    print(f"Removing label: {args.label}")
    output_img_data = np.where(input_img_data == args.label, 0, input_img_data)

    # Display unique labels after modification
    unique_labels_after = np.unique(output_img_data)
    print(f"Unique labels after modification: {unique_labels_after}")
    
    # Create a new Nifti image with the modified data
    # We reuse the original affine (spatial orientation) and header (metadata) for consistency
    output_img_nii = nib.Nifti1Image(output_img_data, input_img_nii.affine, input_img_nii.header)

    # Save the modified image to the specified output file
    print(f"Saving modified image to {args.output}...")
    nib.save(output_img_nii, args.output)
    print("Image saved successfully.")

# Entry point for the script
if __name__ == "__main__":
    """
    This block is executed when the script is run from the command line.
    It calls the main function to process the input image and remove the specified label.
    """
    main()