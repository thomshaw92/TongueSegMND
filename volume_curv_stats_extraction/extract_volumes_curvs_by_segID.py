import os
import numpy as np
import nibabel as nib
import argparse
import vtk
from vtk.util.numpy_support import vtk_to_numpy
import pandas as pd

def create_mesh(data, label):
    """Generate a mesh for the given label using marching cubes."""
    vtk_data = vtk.util.numpy_support.numpy_to_vtk(num_array=data.ravel(), deep=True, array_type=vtk.VTK_FLOAT)
    image = vtk.vtkImageData()
    image.SetDimensions(data.shape)
    image.GetPointData().SetScalars(vtk_data)
    
    contour = vtk.vtkDiscreteMarchingCubes()
    contour.SetInputData(image)
    contour.SetValue(0, label)
    contour.Update()
    
    return contour.GetOutput()

def compute_mean_curvature(mesh):
    """Compute the mean curvature of the mesh."""
    curvature_filter = vtk.vtkCurvatures()  # Corrected class name
    curvature_filter.SetInputData(mesh)
    curvature_filter.SetCurvatureTypeToMean()
    curvature_filter.Update()
    
    curvatures = vtk_to_numpy(curvature_filter.GetOutput().GetPointData().GetScalars())
    mean_curvature = np.mean(curvatures)
    return mean_curvature


def extract_label_volumes_and_curvatures(input_directory):
    results = []

    for file in os.listdir(input_directory):
        if file.endswith("labels.nii.gz"):
            segID = int(file.split('_segID-')[1].split('_')[0])
            filepath = os.path.join(input_directory, file)
            img = nib.load(filepath)
            data = img.get_fdata()
            header = img.header
            voxel_sizes = header.get_zooms()  # Get voxel dimensions (width, height, depth) in mm
            voxel_volume = np.prod(voxel_sizes)  # Calculate the volume of a single voxel

            print(f"Processing file: {file} with segID: {segID}")

            record = {'segID': segID}
            for label in range(1, 5):
                label_data = (data == label).astype(float)
                print(f"Creating mesh for label {label} in segID {segID}")
                mesh = create_mesh(label_data, 1)  # Label set to 1 because label_data is binary
                mean_curvature = compute_mean_curvature(mesh)
                print(f"Calculated mean curvature for label {label}: {mean_curvature}")
                voxel_count = np.sum(label_data)
                volume_mm3 = voxel_count * voxel_volume  # Convert voxel count to physical volume

                record[f'Label_{label}_Volume_mm3'] = volume_mm3
                record[f'Label_{label}_Curvature'] = mean_curvature
            
            results.append(record)
            print(f"Finished processing segID {segID}")

    return pd.DataFrame(results)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Extract and print label volumes and mean curvatures from NIfTI segmentation files.")
    parser.add_argument("directory", type=str, help="Directory containing the NIfTI segmentation files")
    parser.add_argument("--output", type=str, help="Output CSV file path")
    args = parser.parse_args()

    df = extract_label_volumes_and_curvatures(args.directory)
    df.sort_values('segID').to_csv(args.output, index=False)
    print(f"Data saved to {args.output}")
