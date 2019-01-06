from flask import Flask, request, render_template, session, redirect
import numpy as np
import pandas as pd 

app = Flask(__name__)
df = pd.read_csv("/Users/stuartbarker/Documents/betting/prediction.csv")
e1 = df.loc[(df.Div == "E1")]
e1 = e1[['HomeTeam', 'predict']]
e1 = pd.DataFrame(e1)
print(e1.columns.values)
print(type(e1))
@app.route("/")
def home():
    return render_template("home.html", tables=[e1.to_html(classes='data')])
@app.route("/about")
def about():
    return render_template("about.html")
if __name__ == "__main__":
    app.run(debug=True)