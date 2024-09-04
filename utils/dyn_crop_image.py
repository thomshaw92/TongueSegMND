import argparse
import nibabel as nib
import numpy as np

def crop_image_dyn(input_file_name, output_file_name):
    # Load the image
    image = nib.load(input_file_name + '.nii.gz')
    label = nib.load(input_file_name + '_labels.nii.gz')
    # Get the image data
    data = image.get_fdata()
    label_data = label.get_fdata()
    
    # find min and max coordinates for x, y and z
    x_min = np.min(np.where(np.sum(label_data!=0,axis=0)!=0)[0])
    if x_min < 20:
        x_min = x_min - x_min
    else:
        x_min = x_min - 20
    x_max = np.max(np.where(np.sum(label_data!=0,axis=0)!=0)[0]) + 20
    
    
    y_min = np.min(np.where(np.sum(label_data!=0,axis=0)!=0)[1]) 
    if y_min < 20:
        y_min = y_min 
    else: 
        y_min = y_min - 20
    y_max = np.max(np.where(np.sum(label_data!=0,axis=0)!=0)[1]) + 20
    
    z_min = np.min(np.where(np.sum(label_data!=0,axis=1)!=0)[0]) 
    if z_min < 20:
        z_min = z_min 
    else:
        z_min = z_min - 20
    z_max = np.max(np.where(np.sum(label_data!=0,axis=1)!=0)[0]) + 20

    mask = np.zeros(np.shape(image))
    mask[z_min:z_max,x_min:x_max, y_min:y_max] = 1
    
    cropped_data = data*mask
    cropped_label = np.array(label_data*mask, dtype=int)
    
    
    # Create a new image with the cropped data but keep the old header
    cropped_image = nib.Nifti1Image(cropped_data, image.affine, image.header)
    cropped_label = nib.Nifti1Image(cropped_label, label.affine, label.header)
    
    # Save the new image to the output path
    nib.save(cropped_image, output_file_name + '_cropped.nii.gz')
    nib.save(cropped_label, output_file_name + '_cropped_labels.nii.gz')
    
def main():
    
    parser = argparse.ArgumentParser(description='Crop slices from a 3D MRI image.')
    parser.add_argument('-i','--input_file_name', type=str, help='Path to the input image.')
    parser.add_argument('-o','--output_file_name', type=str, help='Path to save the cropped image.')

    args = parser.parse_args()

    crop_image_dyn(args.input_file_name, args.output_file_name)

if __name__ == '__main__':
    main()