REPORT zica_search_source.
*----------------------------------------------------------------------*
*       CLASS cl_zica_search_source DEFINITION
*----------------------------------------------------------------------*
* Object Class (perform a keyword search in object's source)
*----------------------------------------------------------------------*
CLASS cl_zica_search_source DEFINITION.

  PUBLIC SECTION.

*-- source code structure, with line number and code
    TYPES : BEGIN OF type_source,
              number TYPE ad01line,
              line TYPE c LENGTH 255,
            END OF type_source.

*-- table type for source code
    TYPES : type_source_tab TYPE TABLE OF type_source WITH DEFAULT KEY.

*-- result structure, with object, include, line number & content
    TYPES : BEGIN OF type_search_result,
              object TYPE tadir-obj_name,
              include TYPE include.
            INCLUDE TYPE type_source.
    TYPES : END OF type_search_result.

*-- type tabl for search results
    TYPES : type_search_result_tab TYPE TABLE OF type_search_result WITH DEFAULT KEY.

*-- possible program states
    CONSTANTS : BEGIN OF c_prog_states,
                  active VALUE 'A',
                  inactive VALUE 'I',
                END OF c_prog_states.
*-- class constructor
    METHODS : constructor IMPORTING name TYPE tadir-obj_name.

*-- performs search
    METHODS : search IMPORTING term TYPE type_source-line
                     RETURNING value(results) TYPE type_source_tab.

  PRIVATE SECTION.

*-- internal source type (used with READ REPORT statement)
    TYPES : BEGIN OF type_source_internal,
              line TYPE c LENGTH 255,
            END OF type_source_internal.

*-- (this) for recursive calls only
    TYPES : type_zica_search_source TYPE REF TO cl_zica_search_source.

*-- used to search for INCLUDE <include_name> statements
    CONSTANTS : c_include TYPE c LENGTH 7 VALUE 'INCLUDE'.

*-- used to discard INCLUDE TYPE <type name> statements
    CONSTANTS : c_type TYPE c LENGTH 4 VALUE 'TYPE'.

*-- used to discard INCLUDE STRUCTURE <structure name> statements
    CONSTANTS : c_structure TYPE c LENGTH 9 VALUE 'STRUCTURE'.

*-- (this) object name
    DATA : name TYPE tadir-obj_name.

*-- (this) object source
    DATA : source TYPE TABLE OF type_source.

*-- (this) object include list
    DATA : includes TYPE TABLE OF type_zica_search_source.

*-- get source code for (this) object
    METHODS : fill_source.

*-- get include list and source code for (this) object
    METHODS : fill_includes.

*-- performs search for a specific include
    METHODS : search_source IMPORTING
                              object  TYPE tadir-obj_name
                              include TYPE tadir-obj_name OPTIONAL
                              term    TYPE type_source-line
                              source  TYPE type_source_tab OPTIONAL
                            RETURNING
                              value(results) TYPE type_search_result_tab.

ENDCLASS.                    "cl_zica_search_source DEFINITION
*----------------------------------------------------------------------*
*       CLASS cl_zica_search_source IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_zica_search_source IMPLEMENTATION.

  METHOD constructor.
    me->name = name.
    me->fill_source( ).
    me->fill_includes( ).
  ENDMETHOD.                    "constructor

  METHOD search.

*-- searches for term inside (this) object's source code and
*-- save the partial results to export parameter
    APPEND LINES OF me->search_source( object  = me->name
                                       term    = term ) TO results.

*-- searches for term inside every include's source code and
*-- also save the partial results to export parameter
    DATA : include TYPE REF TO cl_zica_search_source.
    LOOP AT me->includes INTO include.
      APPEND LINES OF me->search_source(
          object  = me->name
          include = include->name
          term    = term
          source  = include->source ) TO results.
    ENDLOOP.

  ENDMETHOD.                    "search

  METHOD fill_source.

*-- reads source code
    DATA : t_source_internal TYPE TABLE OF me->type_source_internal.
    READ REPORT me->name
           INTO t_source_internal
          STATE me->c_prog_states-active.

*-- converts from internal to external format
    DATA : source_line_internal TYPE type_source_internal.
    DATA : source_line TYPE me->type_source.
    DATA : t_source TYPE TABLE OF me->type_source.
    LOOP AT t_source_internal INTO source_line_internal.
      source_line-number = sy-tabix.
      source_line-line = source_line_internal-line.
      APPEND source_line TO t_source.
    ENDLOOP.

*-- returns source code table
    me->source = t_source.

  ENDMETHOD.                    "fill_source

  METHOD fill_includes.

    DATA : source_line TYPE me->type_source.
    DATA : condensed_line TYPE me->type_source.
    DATA : splitted_line TYPE TABLE OF tadir-obj_name.
    DATA : include_name TYPE tadir-obj_name.
    DATA : include TYPE REF TO cl_zica_search_source.

    LOOP AT me->source INTO source_line.
      condensed_line = source_line.
      CONDENSE condensed_line-line NO-GAPS.
      IF condensed_line-line(7) EQ me->c_include.
        CLEAR splitted_line[].
        CONDENSE source_line-line.
        SPLIT source_line-line AT space INTO TABLE splitted_line.
        CLEAR condensed_line.
        READ TABLE splitted_line INTO include_name INDEX 2.
        IF sy-subrc IS INITIAL.
          include_name = to_upper( include_name ).
          REPLACE ',' WITH '' INTO include_name.
          REPLACE '.' WITH '' INTO include_name.
          CHECK include_name NE me->c_type AND
                include_name NE me->c_structure.
          CLEAR include.
          CREATE OBJECT include
            EXPORTING
              name = include_name.
          APPEND include TO me->includes.
          APPEND LINES OF include->includes TO me->includes.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "fill_includes

  METHOD search_source.

*-- used to analyse source inside method
    DATA : source_to_analyse TYPE type_source_tab.
    DATA : source_line TYPE type_source.

    IF NOT source[] IS INITIAL.
*-- if external source is provided, use it (includes)
      source_to_analyse = source.
    ELSE.
*-- if not, use (this) source code
      source_to_analyse = me->source.
    ENDIF.

*-- extract search results from source code
    DATA : result TYPE type_search_result.
    LOOP AT source_to_analyse INTO source_line.
      IF source_line-line CS term.
        CLEAR result.
        result-object = object.
        IF NOT include IS INITIAL.
          result-include = include.
        ELSE.
          result-include = object.
        ENDIF.
        result-number = source_line-number.
        result-line = source_line-line.
        APPEND result TO results.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "search_source

ENDCLASS.                    "cl_zica_search_source IMPLEMENTATION

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
  res = obj->search( p_term ).

*-- create an instance of a simple ALV for results
  DATA : alv TYPE REF TO cl_salv_table.
  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = alv
    CHANGING
      t_table      = res.

*-- display results to user
  alv->display( ).
