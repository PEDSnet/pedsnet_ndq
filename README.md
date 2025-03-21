# PEDSnet Network Data Quality

This repository contains the PEDSnet specific implementation of the Network Data Quality (NDQ) script.

## SETUP

To set up your session, please first **open the .Rproj file** which will open a new R session and set the working directory appropriately

Then, open the setup.R file and set up the internal configurations. These include:
1. The name of the institution for which you will be executing the script. This will be used throughout the rest of the process for filtering the data.
2. The current version of the PEDSnet CDM against which you will be executing the script.
3. The schema on your local DBMS where your PEDSnet CDM data is housed
4. The schema on your local DBMS where data quality results should be output.
5. Connection information to connect to your local DBMS
       a. This can be fed into the `initialize_session` function as either a JSON file or as a database connection object created within the R session using a package like DBI.
       b. If you choose to use a JSON, some sample configuration files for popular DBMS are included in the config_files directory.

After all this information has been added, please source the setup.R file. This will load all of the necessary materials into the environment.

## EXECUTION
To execute the script, open driver.R. 

This file is where all of the functions for each of the checks are executed. You may execute them in any order, but please be sure to execute the nearest "PRECOMPUTE TABLES" section **above** the check you would like to execute to ensure necessary tables are loaded onto the database before hand.

There are 3 sections, primarily to reduce load on the database and periodically drop tables that are no longer needed for the computation. After you are finished with all the checks you would like to run in a given section, execute the "CLEANUP CHECKPOINT" to drop the previously precomputed tables.

## FEEDBACK / QUESTIONS

If you have any feedback or questions related to this script or to the package itself, please reach out to wieandk@chop.edu or pedsnetdcc@chop.edu. You can also submit an issue in this GItHub repository and we will respond to you there.
