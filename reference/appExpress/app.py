import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse
from shapely.geometry import Polygon # type: ignore
import plotly.graph_objects as go
import ipywidgets as widgets
from functools import reduce
import operator
import math

from shiny import reactive
from shiny.express import input, render, ui, expressify
from shinywidgets import render_plotly

import json



ui.page_opts(title="Quilt Designer", fillable=True)

# global points_selected
# points_selected = []

@expressify
def left_side(**kwargs):
    with ui.accordion(**kwargs):
        with ui.accordion_panel('Design'):
            # Design
            ui.input_numeric('maxLength','Longest Dimension (cm)', 250)
            ui.input_numeric('baffleHeight','Baffle Height', 2),
            ui.input_numeric('chamberHeight','Max Chamber Height', 2.5),
            ui.input_numeric('chamberWidth','Chamber Width', 15),
            ui.input_numeric('percVertBaffle','% Length with Vertical Baffles', 100),
        with ui.accordion_panel('Materials'):
            # Materials
            ui.input_numeric('FP','Fill Power', 750),
            ui.input_numeric('overstuff','% Overstuff', 10),
            ui.input_numeric('innerWeight','Inner Fabric Weight', 50),
            ui.input_numeric('outerWeight','Outer Fabric Weight', 50),
            ui.input_numeric('baffleWeight','Baffle Material Weight', 25),
            ui.input_numeric('seamAllowance','Seam Allowance', 1)

with ui.sidebar():
    left_side(multiple=False, id="acc_single")


@render.plot
def p():
    np.random.seed(19680801)
    x_rand = 100 + 15 * np.random.randn(437)
    fig, ax = plt.subplots()
    ax.hist(x_rand, int(input.n()), density=True)
    return fig


@reactive.calc
def data():

    # Test data
    x = [0,0,50,50]
    y = [0,100,0,100]

    data = {
    'Baffle Height': input.baffleHeight(),
    'Max Chamber Height': input.chamberHeight(),
    'Chamber Width': input.chamberWidth() ,
    'Fill Power': input.FP() ,
    '% Down Overstuff': input.overstuff(),
    '% Length with Vertical Baffles': input.percVertBaffle(),
    'Inner Fabric Weight': input.innerWeight(),
    'Outer Fabric Weight': input.outerWeight(),
    'Baffle Material Weight': input.baffleWeight(),
    'Seam Allowance': input.seamAllowance(),
        }
 
    # # Convert the dictionary into DataFrame 
    # specs = pd.DataFrame(data)

    # Calculate Area of Inner
    def PolyArea(x,y):
        coords = list(zip(x, y))
        if len(x) >= 4:
            center = tuple(map(operator.truediv, reduce(lambda x, y: map(operator.add, x, y), coords), [len(coords)] * 2)) # find center
            sorted_coords = sorted(coords, key=lambda coord: (-135 - math.degrees(math.atan2(*tuple(map(operator.sub, coord, center))[::-1]))) % 360) # arrange clockwise
            # print(sorted_coords)
            shape = Polygon(sorted_coords)
            # print("Area of Polygon:", shape.area)
        return shape.area
    
    x_ref = x + [-i for i in x]
    y_ref = y + [i for i in y]

    # Max dims
    length = np.max(y) - np.min(y)
    width = np.max(x)*2

    # Full baffle dims
    Hb = input.baffleHeight()
    Hc = input.chamberHeight()
    IWB = input.chamberWidth()
    OWB = (np.sqrt(((IWB/2)**2+(Hc-Hb)**2)*2)/2)*np.pi
    data['chamberCSA'] = (np.pi*IWB*(Hc-Hb)*2/4)/2+(Hb*IWB)  #Baffle chamber cross-sectional area
    data['chamberVol'] = data['chamberCSA'] * length

    data['area'] = PolyArea(x_ref,y_ref)
    data['totalVolume'] = data['area'] * data['Baffle Height'] # Only for non-diff cut
    data['FPmet'] = (data['Fill Power'] * 16.387064) / 28.34952 #CUIN/oz to CUCM/g
    # print(FPmet)
    data['gramsDown'] = (data['totalVolume']/data['FPmet'])
    data['gramsDownAdj'] = data['gramsDown'] * (1 + (data['% Down Overstuff'])/100)

    return data


# ### Output Values---------------------------------------------------
# @render.text  
# def text():
#     return data()

# # Help example
# ui.help_text("you need help")

# ### Baffle Plot---------------------------------------------------
# @render_plotly
# def baffle_plot():
#     Hb = data()['Baffle Height'][0]
#     Hc = data()['Max Chamber Height'][0]
#     IWB = data()['Chamber Width'][0]
#     # OWB = (np.sqrt(((IWB/2)**2+(Hc-Hb)**2)*2)/2)*np.pi
#     # Create the base ellipse.
#     ellipse = Ellipse((IWB/2,0), width=IWB, height=(Hc-Hb)*2,
#                     edgecolor='white', facecolor='none', linewidth=2)

#     # Get the path
#     path = ellipse.get_path()
#     # Get the list of path vertices
#     vertices = path.vertices.copy()
#     # Transform the vertices so that they have the correct coordinates
#     vertices = ellipse.get_patch_transform().transform(vertices)
#     upper_vert = vertices[int(len(vertices)*1/4):int(len(vertices)*3/4)]

#     # Combine verts for plotting
#     x_ell = [item[0] for item in upper_vert]
#     y_ell = [item[1] for item in upper_vert] + Hb
#     x_base = [0,0,IWB,IWB]
#     x = [*x_base, *x_ell]
#     y_base = [Hb,0,0,Hb]
#     y = [*y_base, *y_ell]

#     figBaffle = go.Figure(go.Scatter(x= x,
#                                      y=y, 
#                                      mode = 'lines')
#                                      ).update_traces(showlegend=False)
    
#     figBaffle.add_trace(go.Scatter(
#         x=[-1, -1],
#         y=[0, Hb],
#         mode="lines+text",
#         name="Baffle Height",
#         textposition="bottom center"
#     ))

#     figBaffle.add_trace(go.Scatter(
#         x=[IWB/2, IWB/2],
#         y=[0, Hc],
#         mode="lines+text",
#         name="Chamber Height",
#         textposition="top center"
#     ))

#     figBaffle.add_trace(go.Scatter(
#         x=[0, IWB],
#         y=[-0.25, -0.25],
#         mode="lines+text",
#         name="Inner Fabric Width",
#         textposition="bottom center"
#     ))

#     figBaffle.update_layout(template="plotly_white", height=200)
#     figBaffle.update_yaxes(range=[-0.5, np.max([int(np.ceil(IWB/4)),int(np.ceil(Hc))])]) 

#     return figBaffle

# ## Design Plot---------------------------------------------------
# @render_plotly
# def design_plot():
#     init_max_dim = input.maxLength() # equal height and width, can be optimised for performance
#     init_height = init_max_dim
#     init_width = init_max_dim
#     points_vert = int(init_height * (4/5) + 1)
#     points_hor = int(init_width * (4/5) + 1)
#     fig=go.FigureWidget([
#         go.Scatter(x=np.repeat(np.linspace(0, init_width/2, points_hor), points_hor),
#                 y=np.tile(np.linspace(init_height, 0, points_vert), points_vert),
#                 mode='markers', name=""),
#         go.Scatter(x=[0], y=[0], mode="lines+markers", name="")
#         ])


#     scatter=fig.data[0]
#     line = fig.data[1]
#     scatter.marker.color="rgba(0,0,0,0)"
#     scatter.marker.size=0
#     fig.layout.hovermode='closest'
#     fig.add_vline(x=0, line_width=1, line_dash="dash", line_color="grey")
#     fig.update(layout_showlegend=False)

#     out = widgets.Output(layout={'border': '1px solid black'})
#     out.append_stdout('Output appended with append_stdout\n')

#     # create our callback function
#     @out.capture()
#     def update_point(trace, points, selector):
#         x = list(line.x) + points.xs
#         y = list(line.y) + points.ys
#         line.update(x=x, y=y)
#         #TODO: Update final point x=0 y=max
#         fig.to_dict()["data"][1]
#         # points_selected.append(2)
#         # points_selected = list(zip(fig.to_dict()["data"][1]['x'], fig.to_dict()["data"][1]['y']))
#         print(list(zip(fig.to_dict()["data"][1]['x'], fig.to_dict()["data"][1]['y'])))

#         return points_selected

    
#     scatter.on_click(update_point)

#     print(out.capture)

    # # @out.capture()
    # # def on_reset_clicked(b):
    # #     line.update(x=[], y=[])
    # #     out.clear_output()
    # # @out.capture()
    # # def on_export_clicked(b):
    # #     print(fig.to_dict()["data"][1])

    # # reset.on_click(on_reset_clicked)
    # # export.on_click(on_export_clicked)


    # # # @shinywidgets?

    # # widgets.VBox([widgets.HBox([export]), widgets.VBox([fig, out])])

    # widgets.VBox([fig, out])
    

    # return fig

# # TODO: update
# @render.code
# def info():
#     # points_selected.append(1)
#     # points_selected = [1,2,3]
#     # print(points_selected)
#     # return str([design_plot.widget._data[1]['x'],design_plot.widget._data[1]['y']])
#     return points_selected

# @render.code
# def infoPoly():
#     return str()


# TODO: Look into webgl for plotting