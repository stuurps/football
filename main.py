#First flask

from flask import Flask, request, render_template, session, redirect
import numpy as np
import pandas as pd 

app = Flask(__name__)
df = pd.read_csv("/Users/stuartbarker/Documents/betting/prediction.csv")
e1 = df.loc[(df.Div == "E1")]
e1 = e1[['HomeTeam', 'AwayTeam', 'Date', 'predict']]
e1 = pd.DataFrame(e1)

e2 = df.loc[(df.Div == "E2")]
e2 = e2[['HomeTeam', 'AwayTeam', 'Date', 'predict']]
e2 = pd.DataFrame(e2)

e3 = df.loc[(df.Div == "E3")]
e3 = e3[['HomeTeam', 'AwayTeam', 'Date', 'predict']]
e3 = pd.DataFrame(e3)

print(e1.columns.values)
print(type(e1))
@app.route("/c")
def home():
    return render_template("home.html", tables=[e1.to_html(classes='data')])

@app.route("/l1")
def league1():
    return render_template("league1.html", tables=[e2.to_html(classes='data')])

@app.route("/l2")
def league2():
    return render_template("league2.html", tables=[e3.to_html(classes='data')])

if __name__ == "__main__":
    app.run(debug=True)