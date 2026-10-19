import argparse
import nibabel as nib

def swap_labels(filename, output_filename, label1, label2):
    """Swaps labels in a 3D NIfTI MRI image."""
    img = nib.load(filename)
    data = img.get_fdata()

    new_data = data.copy()

    new_data[data == label1] = label2
    new_data[data == label2] = label1

    new_img = nib.Nifti1Image(new_data, img.affine, img.header)

    nib.save(new_img, output_filename)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Swap labels in a NIfTI MRI image.")
    parser.add_argument('input_file', type=str, help='Input NIfTI file.')
    parser.add_argument('output_file', type=str, help='Output NIfTI file.')
    parser.add_argument('label1', type=int, help='First label to swap.')
    parser.add_argument('label2', type=int, help='Second label to swap.')

    args = parser.parse_args()
    
    swap_labels(args.input_file, args.output_file, args.label1, args.label2)
