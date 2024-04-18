import os
import pandas as pd
import argparse

def rename_files(mapping_file, directory, dry_run):
    # Load the mapping from the provided CSV file
    mapping_df = pd.read_csv(mapping_file)

    # Iterate over the rows of the DataFrame and rename files
    for _, row in mapping_df.iterrows():
        subjid = row['subjid']
        dataset = row['dataset']
        session_id = row['session']
        tongue_id = row['tongue-id']
        segmentation_id = row['segmentation-id']

        # Format the IDs with leading zeros if necessary
        formatted_segmentation_id = str(segmentation_id).zfill(3)
        formatted_tongue_id = str(tongue_id).zfill(3)

        # Construct the old file name pattern
        file_pattern = f"{dataset}_{subjid}_{session_id}_"

        # Find all files that match the pattern
        files_to_rename = [f for f in os.listdir(directory) if f.startswith(file_pattern)]

        # Rename the files
        for file_name in files_to_rename:
            # Identify if it's T1w or T2w and whether it's a label file
            is_label = 'labels' in file_name
            modality = 'T1w' if 'T1w' in file_name else 'T2w'
            label_suffix = '_labels' if is_label else ''

            # Construct the new file name
            new_name = f"{subjid}_{session_id}_{dataset}_segID-{formatted_segmentation_id}_tongueID-{formatted_tongue_id}_{modality}{label_suffix}.nii.gz"
            old_file_path = os.path.join(directory, file_name)
            new_file_path = os.path.join(directory, new_name)

            if dry_run:
                print(f'Would rename {old_file_path} to {new_file_path}')
            else:
                os.rename(old_file_path, new_file_path)
                print(f'Renamed {old_file_path} to {new_file_path}')

    if dry_run:
        print('Dry run complete. No files have been renamed.')
    else:
        print('Renaming complete.')

def main():
    # Set up argument parsing
    parser = argparse.ArgumentParser(description='Rename NIfTI files based on detailed mapping information.')
    parser.add_argument('mapping_file', type=str, help='CSV file with the mapping of subject IDs, session IDs, segmentation IDs, and other details')
    parser.add_argument('directory', type=str, help='Directory where the NIfTI files are located')
    parser.add_argument('--dry_run', action='store_true', help="Do a dry run without renaming files")

    # Parse the arguments
    args = parser.parse_args()

    # Perform renaming or dry run
    rename_files(args.mapping_file, args.directory, args.dry_run)

if __name__ == "__main__":
    main()
