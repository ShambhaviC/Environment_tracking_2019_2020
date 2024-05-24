# Env_tracking_2019_2020

Tracking a continuously changing environment

The folder "analysis/data/exp1_subjective_mean/configuration_files" and "analysis/data/exp2_objective_mean/configuration_files" contain the Excel files that were used to execute the experimental program with the desired reward schedule using PhenoSoft Control.

The folder "analysis/data/raw_data" contains CSV files with raw data from the experimental runs of groups of bats inside flight-cages with four artificial flowers, i.e., nectar-dispensing devices. The files were produced by the software PhenoSoft Control. 

The folder "analysis/data/meta_data" contains the following input CSV files: "ConditionsSerialReversal.csv" and "MasterTableSerialReversal.csv". These files are necessary for the R scripts that stitch the raw data together and analyse them. 
  
The folder "analysis/data/processed_data" contains the following CSV files: "raw_data_all.csv", which is the complete set of raw data from the PhenoSoft software combined with the meta data from the folder "analysis/data/meta_data"; and "Raw_data_bats.csv", which is the set of raw data that were only produced by the activity of the bats. The folder also contains the following RDA files, which are the outputs of the GLMM models in the statistical analyses in this study: "m.firstnight.blockbin.nofirstblock.rda"; "m.firstnight.blockbin.lastthreeblocks.rda"; "m.dayblockbin.laternights.rda". 

The folder "analysis/R" contains the R script load.R. The output of this script are  are processed CSV files, saved in the folder "analysis/data/processed_data". 

The folder "analysis/images" contains the image "cage_schematic.png" which is read into the RMarkdown file. 

The RMarkdown file with the complete text of the paper and the complete code for the analysis of the CSV files in the folder "analysis/data/processed_data" is found in the folder "analysis". 

## 1. Content of configuration files

These files were written to execute the experimental schedule for each day of the experiment in the software PhenoSoft Control. 

## 2. Content of raw files

|Column label |Type     |Description |
|-------------|---------|------------|
|DateTime     |-        |The astronomical date and time for each event of the experiment. 
|IdRFID       |-        |RFID number of a single bat|
|IdLabel			|-        |Short unique identifying label for each bat|
|unitLabel		|-        |Code identifying which reward-dispensing device ('flower') was activated during an event|
|             |LS       |Detections of infra-red beam interruptions without the detection of a transponder number|
|             |Reader   |Detections of transponder numbers without infra-red beam interruptions|
|             |CondMod  |Detections of both a transponder number and an infra-red beam                                    interruption, identified as a nose-poke|
|             |pumpBeh, Empty, Full| Events relating to states of the syringe and its refilling algorithm|
|             |VersuchCS|Events related to the main program, clarified in **SystemMsg**|
|             |exp      |Events related to the programmed reward schedule, clarified in **SystemMsg**|
|eventDuration|-        |Duration of event in milliseconds|
|sense1duration|-       |Total duration of the infra-red beam interruption|
|sense1Events |-	      |Number of interruptions of infra-red beam. When such events happen fast enough (less than 200ms apart) these are registered as a single event, but the number of such short interruptions is given here|
|senseRFIDrecords|-    	|Number of times the transponder number was detected|
|reinforce1value|-		  |Reward (in pump step units, delivered by a stepper motor syringe pump filled with sugar-water)|
|reinforce1Total|-	   |Contents of this column are irrelevant for this experiment|
|reinforce1Account|-	 |Contents of this column are irrelevant for this experiment|
|outFuncLabel |-  	    |Label indicating which 'flower' delivered a reward in response to a nose-poke|
|outLabel     |-       |Contents of this column are irrelevant for this experiment|
|SystemMsg    |-       |Contents of this column are irrelevant for this experiment|
|MsgValue1    |-       |Events in the experimental schedule: 'start' indicating the start of the experimental program; 'end' indicating the end of the experimental program; 'switch' indicating a reversal of reward contingencies between the two flowers of a pair assigned to an individual bat|
|MsgValue2    |-    		|Name of the experimenal configuration file that produced the raw data file|
|MsgValue3    |-       |Contents of this column are irrelevant for this experiment|

## 3. Content of "ConditionsSerialReversal.csv"

This file is user-generated, providing relevant experimental information not present in the raw files.

|Column label|	Description|
|------------|-------------|
|day         |Number of experimental day starting from the first day to the last sequentially|
|Day		 |Number of each day of each stage of the experiment|
|Condition         |Name of each stage of the experiment|
|Group	       |Number of the group of 4 bats that participated in the experiment at the same time in the same cage|
|Cage		     |Cage number|

## 4. Content of "MasterTableSerialReversal.csv"

This file is user-generated and allows mapping the raw csv files to the respective experimental days.

|Column label|	Description|
|------------|-------------|
|day         |Number of experimental day starting from the first day to the last sequentially|
|path        |Path of the raw csv file corresponding to the day|

## 5. Content of "raw_data_all.csv" file

This file is the output of the load.R script which processes the folder of raw csv files, with further information supplied by "ConditionsSerialReversal" and "MasterTableSerialReversal" csv files.

|Column label |Type     |Description |
|-------------|---------|------------|
|IdRFID       |-        |RFID number of a single bat|
|day		      |-        |Number of experimental day starting from the first day to the last sequentially|
|DateTime	    |-        |Astronomical date and time for each event of the experiment|
|unitLabel		|-        |Code identifying which reward-dispensing device ('flower') was activated during an event|
|             |LS       |Detections of infra-red beam interruptions without the detection of a transponder number|
|             |Reader   |Detections of transponder numbers without infra-red beam interruptions|
|             |CondMod  |Detections of both a transponder number and an infra-red beam                                    interruption, identified as a nose-poke|
|             |pumpBeh, Empty, Full| Events relating to states of the syringe and its refilling algorithm|
|             |VersuchCS|Events related to the main program, clarified in **SystemMsg**|
|             |exp      |Events related to the programmed reward schedule, clarified in **SystemMsg**|
|eventDuration|-        |	Duration of event in milliseconds|
|reinforce1value|-        |	Duration of event in milliseconds|
|reinforce1Account|-        |	Duration of event in milliseconds|
|outFuncLabel |-        |Label indicating which 'flower' delivered a reward in response to a nose-poke|
|outLabel     |-        |Contents of this column are irrelevant for this experiment|
|SystemMsg    |-        |Contents of this column are irrelevant for this experiment|
|MsgValue1    |-        |Events in the experimental schedule: 'start' indicating the start of the experimental program; 'end' indicating the end of the experimental program; 'switch' indicating a reversal of reward contingencies between the two flowers of a pair assigned to an individual bat|
|Day          |-        |Number of each day of each stage of the experiment|
|Condition	  |-        |Name of each stage of the experiment|
|Group		    |-        |Number of the group of 4 bats that participated in the experiment at the same time in the same cage|
|Cage         |-        |Cage number|
|IdLabel      |-        |Short unique identifying label for each bat|
  
## 6. Content of "Raw_data_bats.csv" file

This file is a modification of the file "raw_data_all.csv" containing the data only of the visits made by the bats to the flowers assigned to them. 

|Column label |Type     |Description |
|-------------|---------|------------|
|IdRFID       |-        |RFID number of a single bat|
|day		      |-        |Number of experimental day starting from the first day to the last sequentially|
|DateTime	    |-        ||Astronomical date and time for each event of the experiment|
|unitLabel		|-        |Code identifying which reward-dispensing device ('flower') was activated during an event|
|             |LS       |Detections of infra-red beam interruptions without the detection of a transponder number|
|             |Reader   |Detections of transponder numbers without infra-red beam interruptions|
|             |CondMod  |Detections of both a transponder number and an infra-red beam                                    interruption, identified as a nose-poke|
|             |pumpBeh, Empty, Full| Events relating to states of the syringe and its refilling algorithm|
|             |VersuchCS|Events related to the main program, clarified in **SystemMsg**|
|             |exp      |Events related to the programmed reward schedule, clarified in **SystemMsg**|
|eventDuration|-        |Duration of event in milliseconds|
|reinforce1value|-      |	Duration of event in milliseconds|
|reinforce1Account|-        |Duration of event in milliseconds|
|outFuncLabel|-         |Label indicating which 'flower' delivered a reward in response to a nose-poke|
|outLabel	   |-         |Contents of this column are irrelevant for this experiment|
|SystemMsg	 |-         |Contents of this column are irrelevant for this experiment|
|MsgValue1   |-        |Events in the experimental schedule: 'start' indicating the start of the experimental program; 'end' indicating the end of the experimental program; 'switch' indicating a reversal of reward contingencies between the two flowers of a pair assigned to an individual bat|
|Day         |-        |Number of each day of each stage of the experiment|
|Condition	 |-        |Name of each stage of the experiment|
|Group		   |-        |Number of the group of 4 bats that participated in the experiment at the same time in the same cage|
|Cage        |-         |Cage number|
|IdLabel     |-         |Short unique identifying label for each bat|

## 7. Content of "m.firstnight.blockbin.nofirstblock.rda" file

This file is the output of the statistical model described in the RMarkdown file SRL.Rmd in the code chunk "first-night-first-block-removed"

## 8. Content of "m.firstnight.blockbin.lastthreeblocks.rda" file

This file is the output of the statistical model described in the RMarkdown file SRL.Rmd in the code chunk "first-night-first-two-blocks-removed"

## 9. Content of "m.dayblockbin.laternights.rda" file

This file is the output of the statistical model described in the RMarkdown file SRL.Rmd in the code chunk "second-and-third-nights-analysis"


For further information contact: shambhavic21@gmail.com
