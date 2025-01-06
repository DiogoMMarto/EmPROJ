import shutil
import kagglehub
import os

paths = []
paths.append(kagglehub.dataset_download("yasserh/customer-segmentation-dataset")) # Online retail
paths.append(kagglehub.dataset_download("rodsaldanha/arketing-campaign")) # 
paths.append(kagglehub.dataset_download("janiobachmann/bank-marketing-dataset")) # Done
paths.append(kagglehub.dataset_download("uciml/adult-census-income")) # Done
paths.append(kagglehub.dataset_download("yasserh/titanic-dataset")) # Done

for path in paths:
    print(path)
    # copy the data in folder path to the data/ folder
    for file in os.listdir(path):
        shutil.copyfile(path + "/" + file, "data/" + file)
    
