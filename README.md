DESCRIPTION
Algorithm
(1)Data can be found at https://www.kaggle.com/c/bosch-production-line-performance/data
(2)correlation.R computes the features correlation map within station 
(3)export_src_tgt_weight.R calculates how many samples go from one section to another
(4)hcluster_logisticRegress.R uses clustering to find important section and logistc regression for predictive modeling
(4)CSE6242_project_xgboost.py uses xgboost for predictive modeling
Visualization
(1)app.js  uses D3 with bihisankey library to implement node graph for production line; implement correlation map
(2)realtimeApp.js implements the realtime section failure rate 
What you can do with the demo
(1)You may double click, drag, mouseover to explore the production line/sections you are interested in. And detailed information will appear when you mouseover nodes/links.
(3)You may check the dynamically updating section ranking of failure rate. Usually we explore the sections with highest failure rate firstly. 
(4)After you find the section you are interested in, you may check the feature correlation map by choosing specific section in the bottom-left module. 
-------------------------------------------------------------------------------------------------

INSTALLATION
No installation needed. 

-------------------------------------------------------------------------------------------------
EXECUTION
For visulization demo, please run the "d3_code/bosch_bihi/final_app.html" via Firefox. 

