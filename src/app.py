import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from dash import *
from dash.dependencies import Input, Output, State
import dash_bootstrap_components as dbc
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

points_selected = []

dataIn = {
    'Seam Allowance': [1],
    'Baffle Height':[2.5],
    'Max Chamber Height':[2.5],
    'Number of Chambers': [8],
    'Down Fill Rating': [800],
    '% Down Overstuff': [10],
    '% Length with Vertical Baffles': [100],
    'Inner Fabric Weight': [50],
    'Outer Fabric Weight': [50],
    'Baffle Material Weight': [25],
        }

dataOut = {
    'Length': [0],
    'Width': [0],
    'Area': [0],
    'Baffle Width':[0],
    'Total Volume': [0],
    'Grams of Down Required': [0],
    'Final Weight': [0]
            }
 
# Convert the dictionary into DataFrame 
specsIn = pd.DataFrame(dataIn)
specsOut = pd.DataFrame(dataOut)
 


# Baffle Figure
figBaffle = go.Figure(go.Scatter(x=[0], y=[0], fill="toself"))

# Generic dataframe
df = px.data.tips()
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
    # html.H2('Baffle Construction',id='h2_text'),
    dbc.Container([
        dbc.Label('Click a cell in the table:'),
        dash_table.DataTable(specsIn.to_dict('records'),[{"name": i, "id": i} for i in specsIn.columns], id='tblIn', editable=True),
        # dbc.Alert(id='testAlert'),
    ]),
    dbc.Container([
        dash_table.DataTable(specsOut.to_dict('records'),[{"name": i, "id": i} for i in specsOut.columns], id='tblOut', editable=False),
    ]),
    html.Br(),
    # html.Div([
    #     "Differential Cut: ",
    # dcc.RadioItems(['None','Vertical','Horizontal','Both'], 'None', id='diffCut')
    # ]),
    dcc.Graph(id="graphBaffle", figure=figBaffle),
    html.Br(),
    dcc.Graph(id="graphFaceted", figure=figInnerOuter),
     ]
)





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
    # print(x)
    y = [i["y"] for i in points_selected]
    # print(y)
    length = np.max(y) - np.min(y)
    # print("Length: ", length)
    width = np.max(x)*2
    # print("Width: ", width)
    x_ref = x + [-i["x"] for i in points_selected]
    y_ref = y + [i["y"] for i in points_selected]
    # print(PolyArea(x_ref,y_ref))
    fig.data[0].update(x=x_ref, y=y_ref) 
    return

# TODO: Share data from callback/examine use of state for multiple users.
@app.callback(
    Output(component_id='tblOut', component_property='data'),
    # Input(component_id='graph', component_property='clickData'),
    Input(component_id='tblIn', component_property='data'),
    # Include button press to initiate calcs?
)
def updateDataOut(dataIn):
    print(dataIn[0]['Baffle Height'])
    if len(points_selected) > 1:
        x = [i["x"] for i in points_selected]
        y = [i["y"] for i in points_selected]
        x_ref = x + [-i["x"] for i in points_selected]
        y_ref = y + [i["y"] for i in points_selected]

        length = np.max(y) - np.min(y)
        width = np.max(x)*2
        area = PolyArea(x_ref,y_ref)
        baffleWidth = width/dataIn[0]['Number of Chambers']
        totalVolume = float(area) * float(dataIn[0]['Baffle Height']) # Only for non-diff cut
        gramsDown = (totalVolume/dataIn[0]['Down Fill Rating']) 
        gramsDownAdj = gramsDown * (1 + (dataIn[0]['% Down Overstuff']/100))
        print(baffleWidth)
        print(totalVolume)
        print(gramsDown)
        print(gramsDownAdj)
    return

# Graph Showing Dimensions of Single Full-Width Chamber
@app.callback(
    Output(component_id='graphBaffle', component_property='figure'),
    Input(component_id='tblIn', component_property='data'),
    Input(component_id='tblOut', component_property='data'),
)
def plotBaffleDiagram(dataIn, dataOut):
    Hb = dataIn[0]['Baffle Height']
    Hc = dataIn[0]['Max Chamber Height']
    # IWB = dataOut[0]['Width']/dataIn[0]['Number of Chambers']
    # OWB = (np.sqrt(((IWB/2)**2+(Hc-Hb)**2)*2)/2)*np.pi
    # print(OWB)
    # (SQRT(((H10/2)^2+(C12-C11)^2)*2)/2)*PI()
    # fig.data[0].update(x=x_ref, y=y_ref) 
    return


@app.callback(
    Output(component_id='graphFaceted', component_property='figure'),
    Input(component_id='graph', component_property='clickData'),
    
)
def placeholder(clickData):

    return

if __name__ == "__main__":
    app.run_server(debug=True, dev_tools_props_check=True)

