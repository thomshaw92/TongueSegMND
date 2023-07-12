import argparse
import nibabel as nib

def crop_image(input_path, output_path, slices_to_crop):
    # Load the image
    image = nib.load(input_path)

    # Get the image data
    data = image.get_fdata()

    # Crop slices from each dimension
    cropped_data = data
    for axis, (num_slices, from_start) in enumerate(slices_to_crop):
        if from_start:
            cropped_data = cropped_data.take(range(num_slices, cropped_data.shape[axis]), axis=axis)
        else:
            cropped_data = cropped_data.take(range(cropped_data.shape[axis] - num_slices), axis=axis)

    # Create a new image with the cropped data but keep the old header
    cropped_image = nib.Nifti1Image(cropped_data, image.affine, image.header)

    # Save the new image to the output path
    nib.save(cropped_image, output_path)

def main():
    parser = argparse.ArgumentParser(description='Crop slices from a 3D MRI image.')
    parser.add_argument('input_path', type=str, help='Path to the input image.')
    parser.add_argument('output_path', type=str, help='Path to save the cropped image.')
    parser.add_argument('--x_slices', nargs=2, type=int, default=[0, 0],
                        help='Number of slices to be removed from the x-axis and from start (1) or end (0).')
    parser.add_argument('--y_slices', nargs=2, type=int, default=[0, 0],
                        help='Number of slices to be removed from the y-axis and from start (1) or end (0).')
    parser.add_argument('--z_slices', nargs=2, type=int, default=[0, 0],
                        help='Number of slices to be removed from the z-axis and from start (1) or end (0).')

    args = parser.parse_args()

    slices_to_crop = [args.x_slices, args.y_slices, args.z_slices]

    crop_image(args.input_path, args.output_path, slices_to_crop)

if __name__ == '__main__':
    main()
