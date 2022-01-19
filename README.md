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
Open and run the `dataset_construction.ipynb` jupyter notebook in order to construct the dataset needed for the Mixed-Effect Regression Model (Step 2) and the Prophet Model (Step 3). As a side note, inside Step 2 and 3, the dataset will be further adapted (but only slightly) for it to fit the respective format specifications.

# Step 2: Mixed-Effect Regression Model (in R)

# Step 3: Prophet Model (in Python)
Open and run the `Data_Prophet.ipynb` jupyter notebook for a thorough adaptation of the dataset and an analysis of the (users) total traffic volume on the XuetangX platform. We perform a time-series analysis through the [Facebook Library Prophet](https://facebook.github.io/prophet/).

## Reference
```
@inproceedings{feng2019dropout,
title={Understanding Dropouts in MOOCs},
author={Wenzheng Feng and Jie Tang and Tracy Xiao Liu and Shuhuai Zhang and Jian Guan},
booktitle={Proceedings of the 33rd AAAI Conference on Artificial Intelligence},
year={2019}
}
```
