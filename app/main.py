import streamlit as st
import numpy as np
# from shapely.geometry import Polygon # type: ignore
# from functools import reduce
# import operator
# import math
from bokeh.plotting import figure
from bokeh.models import HoverTool, ColumnDataSource, CustomJS, Div
from bokeh.layouts import Column

st.title("Down Quilt Designer")

longest_dim = st.number_input('Longest Dimension (cm)',0,250,0)

st.write('Hold shift and select points to form right side of quilt')

# Function to create an interactive Bokeh chart for plotting design dimensions
def create_plotting_graph(longest_dim):
    num_points = (longest_dim*4) + 1
    x=np.tile(np.linspace(0, longest_dim, num_points), num_points)
    y=np.repeat(np.linspace(longest_dim, 0, num_points), num_points)

    p = figure(title="Plotting Area", tools="tap,reset")
    p.scatter(x, y, line_width=0.1)
    p.toolbar.active_drag = None
    p.toolbar.active_scroll = None
    p.toolbar.active_tap = None
    hover = HoverTool(tooltips=[("x", "@x{1.11}"),
                                ("y", "@y{1.11}")]) 
    p.add_tools(hover)

    return p

plotting_graph = create_plotting_graph(longest_dim)
st.bokeh_chart(plotting_graph, use_container_width=True)

with st.sidebar:
    with st.expander('Design', expanded=True):
        baffle_height = st.number_input('Baffle Height',0,15)
        max_chamber_height = st.number_input('Max Chamber Height',0,20)
        chamber_width =st.number_input('Chamber Width',0,25)
        perc_vert_baff = st.number_input('\% Length with Vertical Baffles',0,100)
    with st.expander('Materials', expanded=False):
        fp = st.number_input('Fill Power', 700, 1000, step=50)
        perc_overstuff = st.number_input('% Overstuff', -50, 50, step=5)
        inner_fab_weight = st.number_input('Inner Fabric Weight', 0, 200)
        outer_fab_weight = st.number_input('Outer Fabric Weight', 0, 200)
        baff_mat_weight = st.number_input('Baffle Material Weight', 0, 200)
        seam_allowance = st.number_input('Seam Allowance', 0.0, 5.0, step=0.25)




# Create some example data
data = {
    "x": [1, 2, 3, 4, 5],
    "y": [5, 4, 3, 2, 1],
}

clicked_data = {"x": None, "y": None}

# Create a Bokeh figure and add a scatter plot
p = figure(title="Click on a dot", tools="tap,reset")
source = ColumnDataSource(data=data)
scatter = p.circle("x", "y", size=20, source=source)
div = Div(text  = str(clicked_data))

c = Column(p, div)


# Create a JavaScript callback function
callback_code = """
    const selected_indices = cb_obj.indices;
    if (selected_indices.length > 0) {
        const selected_index = selected_indices[0];
        const x_value = source.data['x'][selected_index];
        const y_value = source.data['y'][selected_index];
        
        clicked_data.x = x_value;
        clicked_data.y = y_value;
        div.text = '{"x" : ' + x_value + ',  "y" : ' + y_value +'}';
    }
    else{
        div.text = '{"x" : ' + 'None' + ',  "y" : ' + 'None' +'}';
    }
 
    div.change.emit();
"""
callback = CustomJS(
    args={"source": source, "clicked_data": clicked_data, "div" : div}, code=callback_code
)

# Attach the JavaScript callback to the scatter plot
scatter.data_source.selected.js_on_change('indices', callback)


# Display the Bokeh plot in Streamlit
st.bokeh_chart(c, use_container_width=True)