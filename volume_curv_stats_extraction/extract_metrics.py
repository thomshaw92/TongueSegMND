import os
import numpy as np
import nibabel as nib
import pandas as pd

def calculate_volume(label_data, voxel_volume):
    unique_labels, counts = np.unique(label_data, return_counts=True)
    volumes = dict(zip(unique_labels, counts * voxel_volume))
    return volumes

def convert_labels_to_int(dataset_path):
    print(f"Processing dataset at path: {dataset_path}")  # Debug print
    for root, dirs, files in os.walk(dataset_path):
        for file in files:
            if file.endswith('labels.nii.gz'):
                label_file = os.path.join(root, file)
                int_filename = label_file.replace('labels.nii.gz', 'labels_int.nii.gz')
                if os.path.exists(int_filename):  # Check if the _int version already exists
                    print(f"Skipping {file} since integer version exists.")  # Debug print
                    continue
                label_data = nib.load(label_file).get_fdata()
                if np.array_equal(label_data, label_data.astype(int)):
                    print(f"{file} is already in integer format. Saving with _int suffix.")  # Debug print
                    # Save with _int suffix
                    nib.save(nib.Nifti1Image(label_data, nib.load(label_file).affine), int_filename)
                    continue
                print(f"Converting {file} to integer type...")
                label_data = np.round(label_data).astype(np.int32)
                nib.save(nib.Nifti1Image(label_data, nib.load(label_file).affine), int_filename)

def extract_volumes_from_int_labels(dataset_path, dataset_name):
    all_rows = []
    for root, dirs, files in os.walk(dataset_path):
        for file in files:
            if file.endswith('labels_int.nii.gz'):
                print(f"Processing {file}")
                label_file = os.path.join(root, file)
                label_data = nib.load(label_file).get_fdata()

                voxel_dims = nib.load(label_file).header.get_zooms()
                voxel_volume = np.prod(voxel_dims)
                label_volumes = calculate_volume(label_data, voxel_volume)
                
                split_filename = file.split('_')
                subject_id = split_filename[0].replace("sub-", "")
                session_id = split_filename[1].replace("ses-", "")
                for label, volume in label_volumes.items():
                    all_rows.append({
                        'Dataset': dataset_name,
                        'Subject_ID': subject_id,
                        'Session_ID': session_id,
                        'Modality': 'T1w' if 'T1w' in file else 'T2w',
                        'Label': label,
                        'Volume': volume
                    })
    df = pd.DataFrame(all_rows)
    return df


# Paths
belong_path = '/mnt/r/TONGUESEG-Q5346/data/final_analysis_labelled_data/BeLong'
sydney_path = '/mnt/r/TONGUESEG-Q5346/data/final_analysis_labelled_data/Sydney'
eatt_path = '/mnt/r/TONGUESEG-Q5346/data/final_analysis_labelled_data/EATT'

# Convert labels to integer type
convert_labels_to_int(sydney_path)
convert_labels_to_int(eatt_path)
convert_labels_to_int(belong_path)

# Extract volumes from integer label files
belong_metrics = extract_volumes_from_int_labels(belong_path, 'BeLong')
sydney_metrics = extract_volumes_from_int_labels(sydney_path, 'Sydney')
eatt_metrics = extract_volumes_from_int_labels(eatt_path, 'EATT')

# Consolidate and save results
df = pd.concat([belong_metrics, sydney_metrics, eatt_metrics])
df.sort_values(['Dataset', 'Subject_ID', 'Session_ID', 'Modality', 'Label'], inplace=True)
df.to_csv('/mnt/r/TONGUESEG-Q5346/code/TongueSegMND/volume_curv_stats_extraction/results.csv', index=False)
