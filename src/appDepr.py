import numpy as np
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from dash import *
from dash.dependencies import Input, Output
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
# Check adjusted down measurement

## Later
# volume for differential. Calculate per baffle?

# ------------- Define Graphs ---------------------------------------------------
GS = 201
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
fig.update_layout(clickmode="event")

points_selected = []

data = {
    'Seam Allowance': [1],
    'Baffle Height':[2.5],
    'Max Chamber Height':[2.5],
    'Number of Chambers': [8],
    'Fill Power': [800],
    '% Down Overstuff': [10],
    '% Length with Vertical Baffles': [100],
    'Inner Fabric Weight': [50],
    'Outer Fabric Weight': [50],
    'Baffle Material Weight': [25],
    'Length': [0],
    'Width': [0],
    'Area': [0],
    'Baffle Width':[0],
    'Total Volume': [0],
    'Grams of Down Required': [0],
    'Final Weight': [0]
        }

# dataOut = {
#     'Length': [0],
#     'Width': [0],
#     'Area': [0],
#     'Baffle Width':[0],
#     'Total Volume': [0],
#     'Grams of Down Required': [0],
#     'Final Weight': [0]
#             }
 
# Convert the dictionary into DataFrame 
specs = pd.DataFrame(data)
# specsOut = pd.DataFrame(dataOut)
 


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
    dbc.Container([
        dbc.Label('Click a cell in the table:'),
        dash_table.DataTable(specs.to_dict('records'),[{"name": i, "id": i} for i in specs.columns], id='tblIn', editable=True),
    ]),
    html.Br(),
    dbc.Container([
        dbc.Label('Click a cell in the table:'),
        dash_table.DataTable(specs.to_dict('records'),[{"name": i, "id": i} for i in specs.columns], id='tblOut', editable=False),
    ]),
    html.Br(),
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
    # length = np.max(y) - np.min(y)
    # print("Length: ", length)
    # width = np.max(x)*2
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
def updateDataOut(data):
    # print(data[0]['Baffle Height'])
    if len(points_selected) > 1:
        x = [i["x"] for i in points_selected]
        y = [i["y"] for i in points_selected]
        x_ref = x + [-i["x"] for i in points_selected]
        y_ref = y + [i["y"] for i in points_selected]

        length = np.max(y) - np.min(y)
        width = np.max(x)*2
        area = PolyArea(x_ref,y_ref)
        baffleWidth = float(width)/float(data[0]['Number of Chambers'])
        totalVolume = float(area) * float(data[0]['Baffle Height']) # Only for non-diff cut
        FPmet = (float(data[0]['Fill Power']) * 16.387064) / 28.34952 #CUIN/oz to CUCM/g
        # print(FPmet)
        gramsDown = (float(totalVolume)/FPmet)
        gramsDownAdj = gramsDown * (1 + (float(data[0]['% Down Overstuff'])/100))
        print(length)
        print(width)
        print(area)
        print(baffleWidth)
        print(totalVolume)
        print(gramsDown)
        print(gramsDownAdj)
        data[0].update({'Length': float(length),
                         'Width': float(width),
                         'Area': float(area),
                         'Baffle Width': float(baffleWidth),
                         'Total Volume': float(totalVolume),
                         'Grams of Down Required': float(gramsDownAdj)
        }
        )
        # print(data[0])
    return data[0]

# Graph Showing Dimensions of Single Full-Width Chamber
@app.callback(
    Output(component_id='graphBaffle', component_property='figure'),
    Input(component_id='tblIn', component_property='data'),
    # Input(component_id='tblOut', component_property='data'),
)
def plotBaffleDiagram(dataIn):
    Hb = dataIn[0]['Baffle Height']
    Hc = dataIn[0]['Max Chamber Height']
    print(dataIn[0])
    IWB = dataIn[0]['Width']/dataIn[0]['Number of Chambers']
    print(IWB)
    OWB = (np.sqrt(((IWB/2)**2+(Hc-Hb)**2)*2)/2)*np.pi
    print(OWB)
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

