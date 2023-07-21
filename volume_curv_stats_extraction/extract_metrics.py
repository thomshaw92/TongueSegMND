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

def check_and_convert_dtype(label_data):
    if label_data.dtype not in [np.int16, np.int32, np.int64]:
        print(f"Warning: Label data is not of integer type. Current type: {label_data.dtype}. Converting to integer...")
        label_data = label_data.astype(np.int32)
    return label_data
def get_intensity_filename(label_file, modality):
    return label_file.replace("_labels", "")


def process_file(file, root, dataset_name):
    metrics = []
    
    split_filename = file.split('_')
    subject_id = split_filename[0].replace("sub-", "")
    session_id = split_filename[1].replace("ses-", "")
    label_file = os.path.join(root, file)

    label_data = nib.load(label_file).get_fdata()
    label_data = check_and_convert_dtype(label_data)

    voxel_dims = nib.load(label_file).header.get_zooms()
    voxel_volume = np.prod(voxel_dims)

    for label in np.unique(label_data):
        volume = calculate_volume(label_data, voxel_volume).get(label, 0)

        for modality in ['T1w', 'T2w']:
            intensity_file = get_intensity_filename(label_file, modality)
            if not os.path.exists(intensity_file):
                print(f"Warning: {modality} intensity file not found: {intensity_file}")
                continue

            intensity_data = nib.load(intensity_file).get_fdata()
            intensity = calculate_intensity(intensity_data, label_data, label)

            metrics.append({
                'Dataset': dataset_name,
                'Subject_ID': subject_id,
                'Session_ID': session_id,
                'Modality': modality,
                'Label': label,
                'Volume': volume,
                'Intensity': intensity
            })
    return pd.DataFrame(metrics)  # return DataFrame

def calculate_metrics_for_dataset(dataset_path, dataset_name):
    df = pd.DataFrame()
    
    for root, dirs, files in os.walk(dataset_path):
        for file in files:
            if 'labels' in file:
                print(f"Processing {file}")
                file_metrics = process_file(file, root, dataset_name)
                df = pd.concat([df, file_metrics], ignore_index=True)
    return df


belong_path = '/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labelled_data/BeLong'
belong_metrics = calculate_metrics_for_dataset(belong_path, 'BeLong')

sydney_path = '/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labelled_data/Sydney'
sydney_metrics = calculate_metrics_for_dataset(sydney_path, 'Sydney')

eatt_path = '/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labelled_data/EATT'
eatt_metrics = calculate_metrics_for_dataset(eatt_path, 'EATT')

df = pd.concat([belong_metrics, sydney_metrics, eatt_metrics])
df.sort_values(['Dataset', 'Subject_ID', 'Session_ID', 'Modality', 'Label'], inplace=True)
df.to_csv('/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/code/TongueSegMND/volume_curv_stats_extraction/results.csv', index=False)

