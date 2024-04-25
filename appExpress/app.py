import numpy as np
import pandas as pd
from matplotlib.patches import Ellipse
from shapely.geometry import Polygon # type: ignore
import plotly.express as px
import plotly.graph_objects as go
from functools import reduce
import operator
import math

from shiny import reactive
from shiny.express import input, render, ui, expressify
from shinywidgets import render_plotly

import json



# ui.page_opts(title="Quilt Designer", fillable=True)

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





@reactive.calc
def data():

    # Test data
    x = [0,0,50,50]
    y = [0,100,0,100]

    data = {
    'Baffle Height':[input.baffleHeight()],
    'Max Chamber Height':[input.chamberHeight()],
    'Chamber Width': [input.chamberWidth()],
    'Fill Power': [input.FP()],
    '% Down Overstuff': [input.overstuff()],
    '% Length with Vertical Baffles': [input.percVertBaffle()],
    'Inner Fabric Weight': [input.innerWeight()],
    'Outer Fabric Weight': [input.outerWeight()],
    'Baffle Material Weight': [input.baffleWeight()],
    'Seam Allowance': [input.seamAllowance()],
        }
 
    # Convert the dictionary into DataFrame 
    specs = pd.DataFrame(data)

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
    chamberCSA = (np.pi*IWB*(Hc-Hb)*2/4)/2+(Hb*IWB)  #Baffle chamber cross-sectional area
    chamberVol = chamberCSA * length

    area = PolyArea(x_ref,y_ref)
    totalVolume = area * specs['Baffle Height'] # Only for non-diff cut
    FPmet = (specs['Fill Power'] * 16.387064) / 28.34952 #CUIN/oz to CUCM/g
    # print(FPmet)
    gramsDown = (totalVolume/FPmet)
    gramsDownAdj = gramsDown * (1 + (specs['% Down Overstuff'])/100)

    outData = pd.DataFrame({'Length': [length],
                  'Width': [width],
                  'Area': [area],
                  'Chamber Cross-sectional Area': [chamberCSA],
                  'Volume Full Chamber': [chamberVol],
                  'Total Volume': [totalVolume],
                  'Grams of Down Required': [gramsDownAdj]
        }
        )
    
    data = pd.concat([specs,outData], axis = 1)

    return data

@render.data_frame
def table():
    return render.DataGrid(data())

### Output Values---------------------------------------------------
@render.text  
def text():
    return 'placeholder text'

ui.help_text("you need help")

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
#     # init_height = input.maxHeight()
#     # init_width = input.maxWidth()
#     design_x = [0,1,2,3,4]
#     design_y = [0,1,2,3,4]
#     init_max_dim = input.maxLength() # equal height and width, can be optimised for performance
#     init_height = init_max_dim
#     init_width = init_max_dim
#     points_vert = int(init_height * (4/5) + 1)
#     points_hor = int(init_width * (4/5) + 1)
#     fig = px.scatter(x=design_x, y=design_y
#                     #  ).update_traces(marker_color="rgba(0,0,0,0)"
#                                      ).add_traces(
#         px.scatter(
#             x=np.repeat(np.linspace(-init_width/2, init_width/2, points_hor), points_hor),
#             y=np.tile(np.linspace(init_height, 0, points_vert), points_vert)
#         )
#         .update_traces(marker_color="rgba(0,0,0,0)")
#         .data
#     )

#     fig.add_vline(x=0, line_width=3, line_dash="dash", line_color="grey")
#     # fig.update_layout(clickmode="event")

#     selected = fig.data[0]
#     colors = ['#a3a7e4'] * 100
#     selected.marker.color = colors
#     selected.marker.size = [10] * 100
#     fig.layout.hovermode = 'closest'


#     # @out.capture()
#     # def base_click(trace, points, selector):
#     #     global clicked
#     #     clicked.append([points.xs[0],points.ys[0]])
#     #     print(clicked)
#     #     return
#     # selected.on_click(base_click)

#     # # create our callback function
#     # def update_point(trace, points, selector):
#     #     print('test')
#     #     print(points)
#     #     # c = list(selected.marker.color)
#     #     # s = list(selected.marker.size)
#     #     # for i in points.point_inds:
#     #     #     c[i] = '#bae2be'
#     #     #     s[i] = 2000
#     #     #     # test.append(i)
#     #     #     # print(test)
#     #     #     with fig.batch_update():
#     #     #         selected.marker.color = c
#     #     #         selected.marker.size = s


#     # selected.on_click(update_point)
    

#     return fig

# @render.code
# def info():
#     return str([(design_plot.widget._data[1]['x']),(design_plot.widget._data[1]['y'])])

