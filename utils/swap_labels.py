import argparse
import nibabel as nib

def swap_labels(filename, output_filename, label1, label2):
    """
    Swaps labels in a 3D NIfTI MRI image.

    Parameters:
    filename (str): The path to the input NIfTI file.
    output_filename (str): The path where the output NIfTI file with swapped labels should be saved.
    label1 (int): The first label to be swapped.
    label2 (int): The second label to be swapped.
    """
    img = nib.load(filename)
    data = img.get_fdata()

    # copy original data
    new_data = data.copy()

    # swap labels
    new_data[data == label1] = label2
    new_data[data == label2] = label1

    # create a new NIfTI image with the swapped labels
    new_img = nib.Nifti1Image(new_data, img.affine, img.header)

    # save the new image
    nib.save(new_img, output_filename)


if __name__ == "__main__":
    """
    Parses command line arguments and calls the `swap_labels` function to swap labels in a NIfTI image.
    """
    parser = argparse.ArgumentParser(description="Swap labels in a NIfTI MRI image.")
    parser.add_argument('input_file', type=str, help='Input NIfTI file.')
    parser.add_argument('output_file', type=str, help='Output NIfTI file.')
    parser.add_argument('label1', type=int, help='First label to swap.')
    parser.add_argument('label2', type=int, help='Second label to swap.')

    args = parser.parse_args()
    
    swap_labels(args.input_file, args.output_file, args.label1, args.label2)
