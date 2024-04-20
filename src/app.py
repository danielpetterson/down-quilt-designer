import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from dash import *
from dash.dependencies import Input, Output, State
from functools import reduce
import operator
import math
from shapely.geometry import Polygon # type: ignore

##TODO:
# change to update when clicked
# Create line between points
# Change shown units to show width from opposing point, length of section
# Create df for inner/outer coords (x,y,layer)
# Configure lower plots to facet based on layer
# Volume equations based on non-differential (baffle height*area)
# Calculate amount of fill needed based on input (overstuff, FP)

## Later
# volume for differential. Calculate per baffle?

# ------------- Define Graphs ---------------------------------------------------
GS = 201
fig = px.scatter(
    x=[0], y=[0]
# ).update_traces(mode="lines+markers"
                ).add_traces(
        px.scatter(
            x=np.repeat(np.linspace(-200, 200, GS), GS), y=np.tile(np.linspace(250, 0, GS), GS)
        )
        .update_traces(marker_color="rgba(0,0,0,0)")
        .data
    )

fig.layout.height=500
fig.layout.width=750
fig.add_vline(x=0, line_width=3, line_dash="dash", line_color="grey")
fig.update_layout(clickmode="event")
# # fig.update_layout(clickmode="event+select")
# # fig.update_layout(dragmode="drawline")

data = {'Length':[''],
        'Width':[''],
        'Inner Area':[''],
        'Seam Allowance': [1],
        'Baffle Height':[2.5],
        'Max Chamber Height':[2.5],
        'Number of Baffle Chambers': [8],
        'Down Fill Rating': [800],
        '% Down Overstuff': [10],
        'Inner Fabric Weight': [50],
        'Outer Fabric Weight': [50],
        'Baffle Material Weight': [25]
        }
 
# Convert the dictionary into DataFrame 
df = pd.DataFrame(data)
 
# # select two columns
# print(df[['Name', 'Qualification']])

# Generic dataframe
df = px.data.tips()
# Baffle Figure
figBaffle = go.Figure(go.Scatter(x=[0], y=[0], fill="toself"))

# Inner/Outer Fabric Figure
figInnerOuter = px.scatter(df, x="total_bill", y="tip", color="smoker", facet_col="sex")

# Build App
app = Dash(__name__, update_title=None)
app.layout = html.Div([
    dash.html.H1('Down Quilt Calculator'),
    html.Div([
        "Measurement System: ",
    dcc.RadioItems(id='measurement',options=['Metric', 'Imperial'], value='Metric', inline=True)
    ]),
    dash.html.H3('Step One: Define maximum unfilled dimensions'),
    html.Div([
        "Height: ",
        dcc.Input(id='height', value='200', type='number'),
        "Width: ",
        dcc.Input(id='width', value='140', type='number')
    ]),
    dash.html.H3('Step Two: Draw the right side of the quilt'),
    dcc.Graph(id="graph", figure=fig), 
    html.Div(id="init_plot"),
    html.H2('Baffle Construction',id='h2_text'),
    html.Div([
        "Percentage of length with vertical baffles: ",
        dcc.Input(id='vertProp', value='100', type='number')
    ]),
    html.Div([
        "Baffle Width: ",
        dcc.Input(id='bWidth', value='14', type='number')
    ]),
    html.Br(),
    html.Div([
        "Baffle Height: ",
        dcc.Input(id='bHeight', value='2.5', type='number')
    ]),
    html.Br(),
    html.Div([
        "Differential Cut: ",
    dcc.RadioItems(['None','Vertical','Horizontal','Both'], 'None', id='diffCut')
    ]),
    dash.html.H3('Materials'),
    html.Div([
        "Inner Shell Weight: ",
        dcc.Input(id='innerWeight', value='35', type='number'),
        "Outer Shell Weight: ",
        dcc.Input(id='outerWeight', value='35', type='number')
    ]),
    html.Div([
        "Baffle Material Weight: ",
        dcc.Input(id='baffleMaterialWeight', value='25', type='number'),
        "Down Fill Rating: ",
        dcc.Input(id='FP', value='850', min='600', max='1000', step='50', type='number'),
        "Underfill/Overfill %: ",
        dcc.Input(id='overfillPerc', value='0', step='5', type='number'),
    ]),
    dcc.Graph(id="graphBaffle", figure=figBaffle),
    html.Br(),
    dcc.Graph(id="graphFaceted", figure=figInnerOuter),
    html.Div([
        # html.Div(id='innerShellDims'),
        # html.Div(id='innerShellBaffleWidth'),
        # html.Div(id='outerShellDims'),
        # html.Div(id='outerShellBaffleWidth'),
        html.Div(id='totalChamberVol'),
        html.Div(id='totalDown'),
        html.Div(id='finalWeight'),
    ],id="output")
     ]
)



points_selected = []

def PolyArea(x,y):
    coords = list(zip(x, y))
    if len(x) >= 4:
        center = tuple(map(operator.truediv, reduce(lambda x, y: map(operator.add, x, y), coords), [len(coords)] * 2)) # find center
        sorted_coords = sorted(coords, key=lambda coord: (-135 - math.degrees(math.atan2(*tuple(map(operator.sub, coord, center))[::-1]))) % 360) # arrange clockwise
        print(sorted_coords)
        shape = Polygon(sorted_coords)
        print("Area of Polygon:", shape.area)
    return 

# ------------- Define App Interactivity ---------------------------------------------------
@app.callback(
    Output(component_id='init_plot', component_property='children'),
    Input(component_id='graph', component_property='clickData'),
    Input(component_id='height', component_property='value'),
    Input(component_id='width', component_property='value'),
)
def click(clickData, maxHeight, maxWidth):
    if not clickData:
        raise exceptions.PreventUpdate
    points_selected.append({k: clickData["points"][0][k] for k in ["x", "y"]})
    x = [i["x"] for i in points_selected]
    print(x)
    y = [i["y"] for i in points_selected]
    print(y)
    length = np.max(y) - np.min(y)
    print("Length: ", length)
    width = np.max(x)*2
    print("Width: ", width)
    x_ref = x + [-i["x"] for i in points_selected]
    y_ref = y + [i["y"] for i in points_selected]
    print(PolyArea(x_ref,y_ref))
    fig.data[0].update(x=x_ref, y=y_ref) 
    return

# @callback(
#     Output(component_id='bWidthOut', component_property='children'),
#     Input(component_id='vertProp', component_property='value')
# )
# def update_output_bw(input_value):
#     return f'Percentage vertical: {input_value}'

# @callback(
#     Output(component_id='bWidthOut', component_property='children'),
#     Input(component_id='bWidth', component_property='value')
# )
# def update_output_bw(input_value):
#     return f'Baffle Width: {input_value}'

# @callback(
#     Output(component_id='bHeightOut', component_property='children'),
#     Input(component_id='bHeight', component_property='value')
# )
# def update_output_bh(input_value):
#     return f'Baffle Height: {input_value}'

@app.callback(
    Output(component_id='graphFaceted', component_property='figure'),
    Input(component_id='graph', component_property='clickData'),
    Input(component_id='diffCut', component_property='value'),
)
def placeholder(clickData, diffCut):
    if diffCut == None:
        pass
    return

if __name__ == "__main__":
    app.run_server(debug=True, dev_tools_props_check=True)

