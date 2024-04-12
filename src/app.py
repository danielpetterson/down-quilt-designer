import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
import math, json
from dash import *
from dash.dependencies import Input, Output, State

##TODO:
# change to update when clicked
# Mark point when clicked
# Create line between points
# Change shown units to show width from opposing point, length of section



GS = 100
fig = px.scatter(
    x=[0], y=[0]
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
# # fig.update_layout(clickmode="event")
# # fig.update_layout(clickmode="event+select")
# # fig.update_layout(dragmode="drawline")

# Build App
app = Dash(__name__, update_title=None)
app.layout = html.Div([
    dash.html.H1('Down Quilt Calculator'),
    dcc.Graph(id="graph", figure=fig), 
    html.Div(id="init_plot"),
    html.Div(id='empty_space'),
    html.H2('test',id='h2_text'),
    # dcc.Slider(id='slider'),
     ]
)

points_selected = []

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
    width = np.max(x) - np.min(x)
    print("Width: ", width)
    fig.data[0].update(x=x, y=y) 
    return



if __name__ == "__main__":
    app.run_server(debug=True, dev_tools_props_check=True)

