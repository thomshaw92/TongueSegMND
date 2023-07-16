import os
import re
import nibabel as nib
import pandas as pd
import numpy as np

def save_metrics(dataset, metrics):
    df = pd.DataFrame(metrics)
    df.to_csv('/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labels/results.csv', index=False, mode='w')

def get_intensity_data(dataset_path, subject_id, session_id, modality):
    intensity_filename = f"sub-{subject_id}_ses-{session_id}_{modality}.nii.gz"
    intensity_file_path = os.path.join(dataset_path, intensity_filename)
    return nib.load(intensity_file_path).get_fdata() if os.path.exists(intensity_file_path) else None

def calculate_metrics_for_dataset(dataset_path, dataset_name):
    metrics = []
    for filename in os.listdir(dataset_path):
        match = re.match(r'sub-(\d+)_ses-(\d+)_(T\d)w_labels.nii.gz', filename)
        if match is not None:
            subject_id, session_id, modality = match.groups()

            file_path = os.path.join(dataset_path, filename)
            data = nib.load(file_path).get_fdata()

            intensity_data = get_intensity_data(dataset_path, subject_id, session_id, modality)

            for label in range(1, 6):  # Go through labels 1 to 5
                mask = data == label
                volume = np.nan  # Default to NaN
                intensity = np.nan  # Default to NaN
                if np.any(mask):  # Check if mask is not empty
                    volume = np.sum(mask)
                    if intensity_data is not None:
                        intensity = intensity_data[mask].mean()
                else:
                    print(f'Unique values in image data: {np.unique(data)}')
                    print(f'Warning: Label {label} not found in sub-{subject_id}_ses-{session_id}_{modality}')


                metrics.append({
                    'Dataset': dataset_name,
                    'Subject_ID': subject_id,
                    'Session_ID': session_id,
                    'Modality': modality,
                    'Label': label,
                    'Volume': volume,
                    'Intensity': intensity
                })

    return metrics


belong_metrics = calculate_metrics_for_dataset('/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labels/BeLong', 'BeLong')
sydney_metrics = calculate_metrics_for_dataset('/winmounts/uqtshaw/data.cai.uq.edu.au/TONGUESEG-Q5346/data/final_analysis_labels/Sydney', 'Sydney')

all_metrics = belong_metrics + sydney_metrics
save_metrics('all', all_metrics)
