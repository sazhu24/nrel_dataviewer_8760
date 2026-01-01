# Extracting 8760 Load Profiles from NREL Data Viewers

This README documents the standard workflow for extracting 15-minute load profiles from the **ResStock** and **ComStock** Data Viewers and saving them to the appropriate input folders for downstream processing in DER Planner.

---

## Steps for Downloading 15-Minute Timeseries

### 1) Open the appropriate Data Viewer

**ResStock (Residential)**  
https://resstock.nrel.gov/dataviewer?datasetName=vizstock_resstock_amy2018_2024_release_2_by_state_vu

**ComStock (Commercial)**  
https://comstock.nrel.gov/dataviewer/?datasetName=vizstock_comstock_amy2018_r2_2025_ts_by_state_vu

Select the desired **state**, then click **Explore Timeseries**.

---

### 2) Apply filters to generate the desired load profile

Start with the following base selections:
- **Fuel Type:** Electricity  
- **Upgrade:** Select the desired upgrade package  

Then click **Edit Filters** and apply additional constraints as needed, such as:
- **Heating fuel:** Electricity or Natural Gas (defines baseline type)
- Any other relevant building characteristics

These filters define the baseline and upgrade load profiles that will be exported.

---

### 3) Export the timeseries

- Click **Export CSV**
- Select **15 minute resolution**
- Wait for the **Data ready** message and download the file
- Rename the file according to the naming convention (e.g., `U1_ER.csv`)
- Save the file to one of the following input folders:
  - `{STATE}/inputs/ResStock/`
  - `{STATE}/inputs/ComStock/`

---

## ResStock Query Set (Residential)

Use the following queries to generate load profiles for DER Planner’s **residential sector** measures.

---

*ENERGY STAR HP — Electric Backup — Electric Baseline*  
- **File name:** `U1_ER`  
- **Upgrade:** ENERGY STAR heat pump with electric backup  

**Filters:**  
- Heating fuel: Electricity  

---

*ENERGY STAR HP — Electric Backup — Fuel Baseline*  
- **File name:** `U1_FL`  
- **Upgrade:** ENERGY STAR heat pump with electric backup  

**Filters:**  
- Heating fuel: Natural Gas  

---

*ENERGY STAR HP — Fuel Backup — Fuel Baseline*  
- **File name:** `U4_FL`  
- **Upgrade:** ENERGY STAR heat pump with existing system as backup  

**Filters:**  
- Heating fuel: Natural Gas  

---

*Geothermal Heat Pump — Fuel Baseline*  
- **File name:** `U5_FL`  
- **Upgrade:** Geothermal heat pump  

**Filters:**  
- Heating fuel: Natural Gas  
- Hvac has ducts: Yes

---

*Geothermal Heat Pump — Electric Baseline*  
- **File name:** `U5_ER`  
- **Upgrade:** Geothermal heat pump  

**Filters:**  
- Heating fuel: Electricity  
- Hvac has ducts: Yes
---

## ComStock Query Set (Commercial)

Use the following queries to generate load profiles for DER Planner’s **commercial sector** measures.

---

*HP RTU — Electric Backup — Electric Baseline*  
- **File name:** `HP_RTU_EB_ER`  
- **Upgrade:** Variable Speed HP RTU, Electric Backup  

**Filters:**  
- Heating fuel: Electricity  
- HVAC system type: PSZ-AC with electric coil  

---

*HP RTU — Electric Backup — Fuel Baseline*  
- **File name:** `HP_RTU_EB_FL`  
- **Upgrade:** Variable Speed HP RTU, Electric Backup  

**Filters:**  
- Heating fuel: Natural Gas  
- HVAC system type: PSZ-AC with gas coil  

---

*HP RTU — Original Heating Fuel Backup — Fuel Baseline*  
- **File name:** `HP_RTU_FB_FL`  
- **Upgrade:** Variable Speed HP RTU, Original Heating Fuel Backup  

**Filters:**  
- Heating fuel: Natural Gas  
- HVAC system type: PSZ-AC with gas coil  

---

*HP Boiler — Electric Backup — Fuel Baseline*  
- **File name:** `HP_Boiler_EB_FL`  
- **Upgrade:** HP Boiler, Electric Backup  

**Filters:**  
- Heating fuel: Natural Gas  
- HVAC system type: PSZ-AC with gas boiler  

---

*HP Boiler — Fuel Backup — Fuel Baseline*  
- **File name:** `HP_Boiler_FB_FL`  
- **Upgrade:** HP Boiler, Original Heating Fuel Backup  

**Filters:**  
- Heating fuel: Natural Gas  
- HVAC system type: PSZ-AC with gas boiler  

---

*Electric Resistance Boilers — Fuel Baseline*  
- **File name:** `ER_Boiler_FL`  
- **Upgrade:** Electric Resistance Boilers  

**Filters:**  
- Heating fuel: Natural Gas  
- HVAC system type: PSZ-AC with gas boiler  

---
