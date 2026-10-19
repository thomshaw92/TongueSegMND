import vtk
import argparse

def visualize_vtk_surface(file_path):
    """Visualizes a VTK surface mesh."""
    
    reader = vtk.vtkPolyDataReader()
    reader.SetFileName(file_path)
    reader.Update()
    
    mapper = vtk.vtkPolyDataMapper()
    mapper.SetInputData(reader.GetOutput())
    
    actor = vtk.vtkActor()
    actor.SetMapper(mapper)
    
    renderer = vtk.vtkRenderer()
    renderer.AddActor(actor)
    renderer.SetBackground(0.1, 0.1, 0.1)  # Dark background
    
    render_window = vtk.vtkRenderWindow()
    render_window.AddRenderer(renderer)
    
    interactor = vtk.vtkRenderWindowInteractor()
    interactor.SetRenderWindow(render_window)
    
    style = vtk.vtkInteractorStyleTrackballCamera()
    interactor.SetInteractorStyle(style)
    
    print(f"Displaying: {file_path}")
    
    render_window.Render()
    interactor.Start()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Visualize a VTK surface mesh.")
    parser.add_argument("file_path", type=str, help="Path to the VTK file to visualize.")
    
    args = parser.parse_args()
    visualize_vtk_surface(args.file_path)