import os
import re
import nibabel as nib
import pandas as pd
import numpy as np

def save_metrics(dataset, metrics):
    df = pd.DataFrame(metrics)
    df.to_csv('/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labels/results.csv', index=False, mode='w')

def calculate_metrics_for_dataset(dataset_path, dataset_name):
    metrics = []
    for filename in os.listdir(dataset_path):
        match = re.match(r'sub-(\d+)_ses-(\d+)_T(\d)w_labels.nii.gz', filename)
        if match is not None:
            subject_id, session_id, modality = match.groups()

            file_path = os.path.join(dataset_path, filename)
            data = nib.load(file_path).get_fdata()

            intensity_filename = filename.replace('_labels.nii.gz', '.nii.gz')
            intensity_file_path = os.path.join(dataset_path, intensity_filename)
            intensity_data = nib.load(intensity_file_path).get_fdata()

            for label in range(1, 6):  # Go through labels 1 to 5
                mask = data == label

                volume = 0  # Default to 0
                intensity = np.nan  # Default to NaN
                
                if np.any(mask):  # Check if mask is not empty
                    if dataset_name == 'BeLong' and modality == '2':
                        volume = np.sum(mask)
                    if dataset_name == 'BeLong' and modality == '1':
                        if np.any(intensity_data[mask]):  # Check if there are any nonzero intensity values
                            intensity = intensity_data[mask].mean()
                    if dataset_name == 'Sydney' and modality == '1':
                        volume = np.sum(mask)
                        if np.any(intensity_data[mask]):  # Check if there are any nonzero intensity values
                            intensity = intensity_data[mask].mean()

                metrics.append({
                    'Dataset': dataset_name,
                    'Subject_ID': subject_id,
                    'Session_ID': session_id,
                    'Label': label,
                    'Volume': volume,
                    'Intensity': intensity
                })

    return metrics

belong_metrics = calculate_metrics_for_dataset('/90days/uqtshaw/final_analysis_labels/BeLong', 'BeLong')
sydney_metrics = calculate_metrics_for_dataset('/90days/uqtshaw/final_analysis_labels/Sydney', 'Sydney')

all_metrics = belong_metrics + sydney_metrics
save_metrics('all', all_metrics)
