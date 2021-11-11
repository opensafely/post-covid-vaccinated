from cohortextractor import patients
import inspect
import random


def get_variable_definition(codelist, name, return_expecations=None):
    """
    Derives the variable definition depending on the codelist.system
    (and in the case of snomed, also from the codelist name).
    The variables returned are simple binary variables before the index date.
    """
    if codelist.system == "ctv3":
        return patients.with_these_clinical_events(
            codelist, on_or_before="index_date", return_expectations=return_expecations
        )
    if codelist.system == "snomed":
        if name[-8:-4] == "_dmd":
            return patients.with_these_medications(
                codelist,
                on_or_before="index_date",
                return_expectations=return_expecations,
            )
        if name[-13:-4] == "_clinical":
            return patients.with_these_clinical_events(
                codelist,
                on_or_before="index_date",
                return_expectations=return_expecations,
            )
        raise ValueError(
            """'snomed' type codelists must be named with either the suffix '_dmd'
            or '_clinical' to distinguish between medicine and
            clinical type codelists respectively"""
        )
    if codelist.system == "icd10":
        return patients.admitted_to_hospital(
            with_these_diagnoses=codelist, on_or_before="index_date"
        )
    if codelist.system == "opcs4":
        return patients.admitted_to_hospital(
            with_these_procedures=codelist, on_or_before="index_date"
        )


def retrieve_name(var):
    """
    Gets the name of var. Does it from the out most frame inner-wards.
    :param var: variable to get name from.
    :return: string
    """
    for fi in reversed(inspect.stack()):
        names = [
            var_name
            for var_name, var_val in fi.frame.f_locals.items()
            if var_val is var
        ]
        if len(names) > 0:
            # Adds a random number to the name, otherwise there
            # may be duplicate variable names
            return names[0] + str(random.randint(1000, 10000))


def get_codelist_variable(codelists):
    """
    Takes either a list of codelists or a tuple of a
    list of codelists and an expectations definition
    and returns either:
    - a single variable if len(codelists) == 1
    - a combined variable (using OR)
    """
    if type(codelists) is tuple:
        return_expectations = codelists[1]
        codelists = codelists[0]
    else:
        return_expectations = {}
    string_list = [retrieve_name(v) for v in codelists]
    if len(codelists) > 1:
        logic = " OR ".join(string_list)
        sub_var_dict = {
            f"{s}": get_variable_definition(c, s)
            for s, c in zip(string_list, codelists)
        }
        return (
            patients.satisfying(logic, **sub_var_dict)
            if not return_expectations
            else patients.satisfying(
                logic, **sub_var_dict, 
                return_expectations=return_expectations
            )
        )

    return (
        get_variable_definition(codelists[0], string_list[0])
        if not return_expectations
        else get_variable_definition(
            codelists[0], string_list[0],
            return_expectations)
    )
