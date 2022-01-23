# Step 1: Build the XuetangX MOOC dataset (in Python)

To build the original dataset, we use the code from the following AAAI'19 paper:

Wenzheng Feng, Jie Tang, Tracy Xiao Liu, Shuhuai Zhang, Jian Guan. [Understanding Dropouts in MOOCs](http://keg.cs.tsinghua.edu.cn/jietang/publications/AAAI19-Feng-dropout-moocs.pdf). In Proceedings of the 33rd AAAI Conference on Artificial Intelligence (AAAI'19).

## Download a pre-process dataset

```bash
# download data from www.moocdata.org
sh dump_data.sh

# extract basic activity features from log file
python feat_extract.py

# integrate different types of features
python preprocess.py

# run CFIN model
python main.py
```

## Build dataset
Open and run the `dataset_construction_MELR.ipynb` jupyter notebook in order to construct the dataset and the features for the Mixed-Effect Logistic Regression Model (Step 2) and the `dataset_construction_Prophet.ipynb` jupyter notebook for the Prophet Model (Step 3). As a side note, inside Step 2 and 3, the dataset will be further adapted (but only slightly) for it to fit the respective format specifications.

# Step 2: Mixed-Effect Logistic Regression Model (in R)
Open and run the `MELR_Code.R` R-notebook file to obtain the four different MELR models and the corresponding coefficient estimates including their Bonferroni adapted p-values.

# Step 3: Prophet Model (in Python)
Open and run the `Data_Prophet.ipynb` jupyter notebook for a thorough adaptation of the dataset and an analysis of the (student) total traffic volume on the XuetangX platform. We perform a time-series analysis through the [Facebook Library Prophet](https://facebook.github.io/prophet/).
