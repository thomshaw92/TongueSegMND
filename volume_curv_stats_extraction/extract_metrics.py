import os
import numpy as np
import nibabel as nib
import pandas as pd
from scipy.ndimage import measurements

def calculate_volume(label_data, voxel_volume):
    unique_labels, counts = np.unique(label_data, return_counts=True)
    volumes = dict(zip(unique_labels, counts * voxel_volume))
    return volumes

def calculate_intensity(intensity_data, label_data, label):
    mask = label_data == label
    intensity = intensity_data[mask].mean()
    return intensity

def calculate_metrics_for_dataset(dataset_path, dataset_name):
    metrics = []
    
    for root, dirs, files in os.walk(dataset_path):
        for file in files:
            if 'labels' in file:
                subject_id = file.split('_')[1]
                session_id = file.split('_')[2]
                modality = file.split('_')[3].split('.')[0]
                label_file = os.path.join(root, file)
                
                label_data = nib.load(label_file).get_fdata()
                
                print(f"Processing {file}")
                print(f"Label data type: {label_data.dtype}")
                unique_values, counts = np.unique(label_data, return_counts=True)
                print(f"Unique values in label data: {unique_values}")

                if label_data.dtype != np.int32:
                    print("Warning: Label data is not integer type. Converting to integer...")
                    label_data = label_data.astype(np.int32)
                    unique_values, counts = np.unique(label_data, return_counts=True)
                    print(f"Unique values in label data after conversion: {unique_values}")
                
                voxel_dims = nib.load(label_file).header.get_zooms()
                voxel_volume = np.prod(voxel_dims)
                
                for label in range(1, 6):
                    if label not in unique_values:
                        print(f"Warning: Label {label} not found in {file}")
                        continue
                    
                    volume = calculate_volume(label_data, voxel_volume).get(label, 0)
                    
                    split_filename = label_file.split("_")
                    split_filename[3] = 'T1w.nii.gz'
                    intensity_file = "_".join(split_filename)
                    if not os.path.exists(intensity_file):
                        print(f"Warning: Intensity file not found: {intensity_file}")
                        continue
                    
                    intensity_data = nib.load(intensity_file).get_fdata()
                    intensity = calculate_intensity(intensity_data, label_data, label)
                    
                    metrics.append({
                        'Dataset': dataset_name,
                        'Subject_ID': subject_id,
                        'Session_ID': session_id,
                        'Modality': 'T1w' if dataset_name in ['Sydney', 'EATT'] else 'T2w',
                        'Label': label,
                        'Volume': volume,
                        'Intensity': intensity
                    })
    return metrics

belong_path = '/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labels/BeLong'
belong_metrics = calculate_metrics_for_dataset(belong_path, 'BeLong')

sydney_path = '/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labels/Sydney'
sydney_metrics = calculate_metrics_for_dataset(sydney_path, 'Sydney')

eatt_path = '/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labels/EATT'
eatt_metrics = calculate_metrics_for_dataset(eatt_path, 'EATT')

metrics = belong_metrics + sydney_metrics + eatt_metrics

# Only create DataFrame and sort if there are metrics data
if metrics:
    df = pd.DataFrame(metrics)
    df.sort_values(['Dataset', 'Subject_ID', 'Session_ID', 'Modality', 'Label'], inplace=True)
    df.to_csv('/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/code/TongueSegMND/volume_curv_stats_extraction/results.csv', index=False)
else:
    print("No metrics data to process.")
