import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import math, json
from dash import *
from dash.dependencies import Input, Output, State

##TODO:
# change to update when clicked
# Create line between points
# Change shown units to show width from opposing point, length of section
# Fix PolyArea

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

# df = px.data.tips()
# fig2 = px.scatter(df, x="total_bill", y="tip", color="smoker", facet_col="sex")

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
    # html.Div([
    #     "Percentage of length with vertical baffles: ",
    #     dcc.Input(id='vertProp', value='50', type='number')
    # ]),
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
    dcc.RadioItems(['None','Vertical','Horizontal','Both'], 'None')
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
        dcc.Input(id='FP', value='850', min='700', max='1000', step='50', type='number'),
        "Underfill/Overfill %: ",
        dcc.Input(id='overfillPerc', value='0', step='5', type='number'),
    ]),
    html.Br(),
    # dcc.Graph(id="graph", figure=fig2),
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

#Fix
def PolyArea1(x_coords,y_coords):
    n = len(x_coords)
    area = 0
    for i in range(n):
        x1, y1 = x_coords[i], y_coords[i]
        x2, y2 = x_coords[(i + 1) % n], y_coords[(i + 1) % n]
        area += (x1 * y2 - x2 * y1) #first edge of the triangle
    area = abs(area) / 2 # area of polygon
    print("Area of Polygon:", area)
    return

def PolyArea2(x,y):
    return 0.5*np.abs(np.dot(x,np.roll(y,1))-np.dot(y,np.roll(x,1)))

# ------------- Define App Interactivity ---------------------------------------------------
@app.callback(
    Output(component_id='init_plot', component_property='children'),
    Input(component_id='graph', component_property='clickData'),
)
def click(clickData):
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
    print(x_ref)
    print(y_ref)
    print(PolyArea1(x_ref,y_ref))
    print(PolyArea2(x_ref,y_ref))
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


if __name__ == "__main__":
    app.run_server(debug=True, dev_tools_props_check=True)

