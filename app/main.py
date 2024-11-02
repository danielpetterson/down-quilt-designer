import streamlit as st
import numpy as np
# from shapely.geometry import Polygon # type: ignore
# import plotly.graph_objects as go
# import ipywidgets as widgets
# from functools import reduce
# import operator
# import math

from random import random

# from bokeh.layouts import row
# from bokeh.models import ColumnDataSource, CustomJS
from bokeh.models import HoverTool
from bokeh.plotting import figure, show

st.title("Down Quilt Designer")

longest_dim = st.number_input('Longest Dimension (cm)',0,250,0)

st.write('Hold shift and select points to form right side of quilt')

# Function to create an interactive Bokeh chart
def create_plotting_graph(longest_dim):
    num_points = (longest_dim*4) + 1
    x=np.tile(np.linspace(0, longest_dim, num_points), num_points)
    y=np.repeat(np.linspace(longest_dim, 0, num_points), num_points)

    p = figure(title="Plotting Area", x_axis_label='X-Axis', y_axis_label='Y-Axis')
    p.scatter(x, y, line_width=0.1)
    p.toolbar.active_drag = None
    p.toolbar.active_scroll = None
    p.toolbar.active_tap = None
    # hover = HoverTool(tooltips=[("x", "$x{1.1}"),
    #                             ("y", "$y{1.1}")]) 
    hover = HoverTool(tooltips=[("x", "@x{1.11}"),
                                ("y", "@y{1.11}")]) 
    p.add_tools(hover)

    return p

# num_points = st.slider("Select number of points", 10, 100, 50)
plotting_graph = create_plotting_graph(longest_dim)
st.bokeh_chart(plotting_graph)

with st.sidebar:
    with st.expander('Design', expanded=True):
        st.number_input('Baffle Height',0,15)
        st.number_input('Max Chamber Height',0,20)
        st.number_input('Chamber Width',0,25)
        st.number_input('\% Length with Vertical Baffles',0,100)
    with st.expander('Materials', expanded=False):
        st.number_input('Fill Power', 700, 1000, step=50)
        st.number_input('% Overstuff', -50, 50, step=5)
        st.number_input('Inner Fabric Weight', 0, 200)
        st.number_input('Outer Fabric Weight', 0, 200)
        st.number_input('Baffle Material Weight', 0, 200)
        st.number_input('Seam Allowance', 0.0, 5.0, step=0.25)