from flask import Flask, request


app = Flask(__name__)

@app.route("/", methods=["POST", "GET"])
def transfer():
    if request.method=="GET":
        return "HEY GREG"
    else:
        model_name = request.form.get("modelName", "") 
        model_img = request.files.get("image")
        model_img.save("files/%s.img" % (model_name))
        print model_name

        return "SUCCESS"    

if __name__=="__main__":
    app.run(debug=True)



