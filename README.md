# ABAP-BP
ABAP RAP Service for Business Partner

The ABAP-BP project is a collection of ABAP source code developed for learning and experimenting with Business Partner (BP) functionality in SAP.
This repository includes ABAP objects such as Function Modules, CDS Views, Behavior Definitions (BDEF), Metadata, and OData Services designed according to the RAP (ABAP RESTful Application Programming) model.

---

## Project Objectives
- Create and manage Business Partners (BP) in SAP using ABAP.
- Practice developing BAPIs, Function Modules, and CDS View Entities with associations between main tables such as:
  - `BUT000` – BP: General data I
  - `BUT020` – BP: Addresses
  - `BUT100` – Roles 
  - `ADRC` – Address
- Implement search helps using tables such as:
  - `TSAD3T` – Titles {Mr, Ms, Comp}
  - `TB001` – Grouping Key (BU_GROUP)
  - `TB003` – BP Roles
- Standard BAPIs for BP include:
  - BAPI_BUPA_CREATE_FROM_DATA
  - BAPI_BUPA_ADDRESS_ADD
  -	BAPI_BUPA_CENTRAL_CHANGE
  -	BAPI_BUPA_ROLE_ADD_2
  -	BAPI_BUPA_ROLE_CHANGE
  -	BAPI_BUPA_ROLE_REMOVE
  -	...

---

## Directory Structure

```plaintext
ABAP-BP/
├── Business Service/
│   ├── Service Binding/                
│   └── Service Definition/                  
├── Core Data Service/              
│   ├── Behavior Definition/                # Behavior Definition and Implementation
│   └── Data Definition/                    # CDS View Entities: Z_I_ROOT, Z_I_ROLE, Z_C_ROOT
│   └── Metadata Extension/                
├── Source Code Library/              
│   └── Classes/  
└── README.md
