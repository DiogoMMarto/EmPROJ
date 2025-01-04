import shutil
import kagglehub
import os

paths = []
paths.append(kagglehub.dataset_download("yasserh/customer-segmentation-dataset"))
paths.append(kagglehub.dataset_download("rodsaldanha/arketing-campaign"))
paths.append(kagglehub.dataset_download("janiobachmann/bank-marketing-dataset"))
paths.append(kagglehub.dataset_download("uciml/adult-census-income"))
paths.append(kagglehub.dataset_download("brendan45774/test-file"))
paths.append(kagglehub.dataset_download("yasserh/titanic-dataset"))

for path in paths:
    print(path)
    # copy the data in folder path to the data/ folder
    for file in os.listdir(path):
        shutil.copyfile(path + "/" + file, "data/" + file)
    
