import nibabel as nib
import numpy as np
import argparse
import os
import matplotlib.pyplot as plt

def compute_volume_similarity(label_A, label_B):
    volume_A = np.sum(label_A > 0)
    volume_B = np.sum(label_B > 0)
    return 1 - (abs(volume_A - volume_B) / (volume_A + volume_B))

def compute_dice_score(label_A, label_B):
    intersection = np.sum((label_A > 0) & (label_B > 0))
    volume_A = np.sum(label_A > 0)
    volume_B = np.sum(label_B > 0)
    return (2 * intersection) / (volume_A + volume_B)

def plot_results(results, output_file):
    labels = list(results.keys())
    volume_similarities = [pair_results['volume_similarity'] for pair_results in results.values()]
    dices = [pair_results['dice'] for pair_results in results.values()]

    fig, ax = plt.subplots(2, 1, figsize=(10, 8))

    # Volume Similarity Plot
    ax[0].barh(labels, volume_similarities, color='skyblue')
    ax[0].set_title('Volume Similarity')
    ax[0].set_xlim(0, 1)

    # Dice Score Plot
    ax[1].barh(labels, dices, color='salmon')
    ax[1].set_title('DICE Score')
    ax[1].set_xlim(0, 1)

    plt.tight_layout()
    plt.savefig(output_file)
    plt.show()

def main(args):
    data_folder = args.data_folder
    suffix = args.suffix
    output_file = args.output_file

    filepaths = [os.path.join(data_folder, f) for f in os.listdir(data_folder) if f.endswith(suffix)]
    labels = [1, 2, 3, 4]

    results = {}

    for filepath in filepaths:
        nifti_data = nib.load(filepath).get_fdata()

        for i, label_i in enumerate(labels):
            for j, label_j in enumerate(labels):
                if i < j:  # Avoid computing the same pair twice
                    label_data_i = (nifti_data == label_i)
                    label_data_j = (nifti_data == label_j)

                    vol_sim = compute_volume_similarity(label_data_i, label_data_j)
                    dice = compute_dice_score(label_data_i, label_data_j)

                    pair_key = (label_i, label_j)
                    results[pair_key] = {"volume_similarity": vol_sim, "dice": dice}

    plot_results(results, output_file)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Compute Volume Similarity and DICE scores for labeled nifti data.")
    parser.add_argument("--data-folder", required=True, help="Location of the folder containing the nifti data.")
    parser.add_argument("--suffix", required=True, help="Suffix for the labeled data files.")
    parser.add_argument("--output-file", required=True, help="Filename for the output plot.")
    args = parser.parse_args()

    main(args)
