#usage: python remove-unconnected-components.py input.nii output.nii --labels 1 2 3 4 5
import numpy as np
import argparse
import nibabel as nib
from scipy.ndimage import label, sum
import matplotlib.pyplot as plt
def get_largest_component(img, label_num):
    binary_img = np.where(img == label_num, 1, 0)
    labeled_array, num_features = label(binary_img)

    if num_features < 1:
        return binary_img

    component_sizes = sum(binary_img, labeled_array, range(num_features + 1))
    largest_components_labels = component_sizes.argsort()[-2:][::-1]

    largest_component_img = np.zeros_like(binary_img)

    if len(largest_components_labels) > 1 and component_sizes[largest_components_labels[0]] * 0.55 <= component_sizes[largest_components_labels[1]]:
        print(f"Label {label_num} has two large components with sizes {component_sizes[largest_components_labels[0]]} and {component_sizes[largest_components_labels[1]]} voxels. Keeping both.")
        largest_component_img = np.where((labeled_array == largest_components_labels[0]) | (labeled_array == largest_components_labels[1]), label_num, 0)
    else:
        largest_component_img = np.where(labeled_array == largest_components_labels[0], label_num, 0)

    return largest_component_img


def process_image(img, labels):
    unique_labels = np.unique(img)
    final_img = np.zeros_like(img)
    
    for label in unique_labels:
        if label == 0 or label not in labels:
            continue
        largest_component_img = get_largest_component(img, label)
        final_img = np.where(largest_component_img == label, label, final_img)
    
    return final_img

def parse_arguments():
    parser = argparse.ArgumentParser(description='Find and keep only the largest connected component for the specified labels in a MRI image.')
    parser.add_argument('input', type=str, help='Input Nifti image file path')
    parser.add_argument('output', type=str, help='Output Nifti image file path')
    parser.add_argument('--labels', type=int, nargs='+', default=None, help='List of labels to process. If not provided, all labels are processed.')
    args = parser.parse_args()
    return args

def main():
    args = parse_arguments()

    # Load the input Nifti image
    input_img_nii = nib.load(args.input)
    input_img_data = input_img_nii.get_fdata()

    # Process labels
    labels = args.labels if args.labels is not None else np.unique(input_img_data)
    output_img_data = process_image(input_img_data, labels)

    # Save the output image
    output_img_nii = nib.Nifti1Image(output_img_data, input_img_nii.affine, input_img_nii.header)
    nib.save(output_img_nii, args.output)

if __name__ == "__main__":
    main()

