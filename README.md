ZICA_SEARCH_SOURCE
==================

ZICA is an acronym for Z Instant Comprehensive ABAP example. It's also a funny slang in Portuguese.

This example shows you how to use ABAP OO and recursion to lookup for a term inside the source code of any ABAP report, module pool, function pool or subroutine pool as well. The results are shown in an simple ALV output.

To use ZICA_SEARCH_SOURCE, you'll need to copy it's code to your SAP instance. After that, you can create your own report using the code as below:

```abap
REPORT zica_search_source.

*-- program name (use prefix SAPL<name> for function groups)
PARAMETERS p_prog
      TYPE tadir-obj_name OBLIGATORY.

*-- search term
PARAMETERS p_term
      TYPE cl_zica_search_source=>type_source-line OBLIGATORY.

START-OF-SELECTION.

*-- create an instance of our main object class
  DATA obj TYPE REF TO cl_zica_search_source.
  CREATE OBJECT obj
    EXPORTING
      name = p_prog.

*-- searches for term
  DATA res TYPE cl_zica_search_source=>type_search_result_tab.
  CALL METHOD obj->search
    EXPORTING
      term    = p_term
    CHANGING
      results = res.

*-- create an instance of a simple ALV for results
  DATA : alv TYPE REF TO cl_salv_table.
  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = alv
    CHANGING
      t_table      = res.

*-- display results to user
  alv->display( ).
```

PS: I am aware of RPR_ABAP_SOURCE_SCAN (if you're not, try it!). This piece of code was created for educational purposes. Anyway, the code works and you can use it as production software if you want to.
