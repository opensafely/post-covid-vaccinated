# post-covid-vaccinated

This is the code and configuration for the `post-covid-vaccinated` repository.

You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/post-covid-vaccinated)

## Protocol
The protocol is available [here](https://uob.sharepoint.com/:w:/r/teams/grp-ehr/_layouts/15/Doc.aspx?sourcedoc=%7BEF4D8C0D-B811-4A56-9A03-030E6A32DCC5%7D&file=post-covid-vaccinated.docx&action=default&mobileredirect=true) (currently restricted access).

## Repository navigation
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/).

* Analyses scripts are in the `analysis/` directory:
    * If you are interested in how we defined our variables, we use three study definition scripts for variables specific to our vaccinated cohort [study definition vaccinated](analysis/study_definition_vaccinated.py), variables specific to our electively unvaccinated cohort [study definition electively unvaccinated](analysis/study_definition_electively_unvaccinated.py) and to define common variables and index_date [study definition index](analysis/study_definition_index.py); this is written in `python`. The resulting  extracted data is combined to create our final data extracts in [preprocess](analysis/preprocess_data.R).
    * This directory also contains all the R scripts that process, describe, and analyse the extracted data.

* The `lib/` directory contains preliminary (pre data extract) scripts, useful functions, and dummy data.

* The `project.yaml` defines run-order and dependencies for all the analysis scripts. This file should not be edited directly. To make changes to the yaml, edit and run the `create_project_actions.R` script (available in the `analysis/` directory) which generates all the actions of the `project.yaml`.

* Descriptive and Model outputs, including figures and tables are in the `released_outputs/` directory.


## Manuscript
A first manuscript is being drafted.

## About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality. Read more at [OpenSAFELY.org](https://opensafely.org).

Developers and epidemiologists interested in the framework should review [the OpenSAFELY documentation](https://docs.opensafely.org).

## Licences
As standard, research projects have a MIT license. 
