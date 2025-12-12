# Down Quilt Designer

This application extends the functionality of [CatSplat's Underquilt Calculator](https://catsplat.github.io/underquilt-calculator/) by enabling users to define non-rectangular shapes for custom quilt designs. It provides a user-friendly interface to input shape vertices and customise baffle parameters, generating a visual guide for sewing.

## Features
- Input custom non-rectangular shapes by specifying vertex coordinates.
- Customize baffle orientation, height, and chamber dimensions.
- Visualize the shape with a plot showing outer edges and baffle sewing guides.
- Supports both vertical and horizontal chamber orientations with adjustable parameters.

## How to Use
1. Navigate to the **Input Dimensions** page.
2. Enter the coordinates for each vertex of the right side of your desired shape.
3. Use the sidebar to customize parameters (see **Term Definitions** below for details).
4. Review the generated plot:
   - **Blue circles**: Represent the outer edge vertices of the shape, including seam allowance.
   - **Red circles**: Indicate the vertices of the chambers, guiding where to sew baffles.

## Term Definitions
- **Longest Dimension**: Sets the axis limits for the Input Dimensions graph.
- **Baffle Orientation Change Height**: Divides the shape into vertical chambers above this height and horizontal chambers below. For a single chamber orientation, set this to the minimum or maximum y-value.
- **Baffle Height**: The height of the finished baffle, excluding seam allowance. Set to zero for sewn-through baffles.
- **Max Vertical/Horizontal Chamber Height**: The distance from the chamber's center base to the highest point of the semi-ellipse. For a non-differential cut, set this equal to the baffle height.
- **Vertical/Horizontal Chamber Width**: The maximum distance between baffle walls.

## Contributing
Contributions are welcome! Please submit a pull request or open an issue to discuss improvements or bug fixes.

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgments
This project builds upon the foundational work of [CatSplat's Underquilt Calculator](https://docs.google.com/spreadsheets/d/1dydFlElQQpxNc8OtngQ3qnrHhIug8dUvxYCCkgoIkVs/edit?gid=0#gid=0). 
