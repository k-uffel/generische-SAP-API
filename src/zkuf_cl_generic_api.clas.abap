class ZKUF_CL_GENERIC_API definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IV_API_STRUCTURE_ID type SYSUUID_C22
      !IV_ROOT_WHERE type STRING optional .
  methods GET_JSON
    returning
      value(RV_JSON) type STRING
    raising
      CX_SY_STRUCT_COMP_NAME .
protected section.
private section.

  data MT_API_STRUCTURE type ZKUF_TT_API_STRUCTURE .
  data MV_ROOT_TABLE_NAME type TABNAME .

  methods GET_CONTENT_OF_REQ_TABLES
    importing
      !IT_API_STRUCTURE type ZKUF_TT_API_STRUCTURE
      !IV_ROOT_WHERE type STRING optional
    returning
      value(RT_API_STRUCTURE) type ZKUF_TT_API_STRUCTURE .
  methods BUILD_DYNAMIC_WHERE_CLAUSE
    importing
      !IT_API_STRUCTURE type ZKUF_TT_API_STRUCTURE
      !IV_HOST_VARIABLE type STRING
    returning
      value(RT_API_STRUCTURE) type ZKUF_TT_API_STRUCTURE .
  methods BUILD_JSON
    importing
      !IT_API_STRUCTURE type ZKUF_TT_API_STRUCTURE
    returning
      value(RV_JSON) type STRING
    raising
      CX_SY_STRUCT_COMP_NAME .
  methods GENERATE_STRUCTURE
    importing
      !IT_API_STRUCTURE type ZKUF_TT_API_STRUCTURE
      !IV_DEPTH type I
      !IV_PARENT type TABNAME
    returning
      value(RT_API_DATA_STRUCTURE) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
    raising
      CX_SY_STRUCT_COMP_NAME .
  methods MAP_DATA_TO_STRUCTURE
    importing
      !IT_API_STRUCTURE type ZKUF_TT_API_STRUCTURE
      !IS_COMP_STRUC type DATA
      !IV_DEPTH type I
    exporting
      !ES_COMP_STRUC_W_DATA type DATA .
ENDCLASS.



CLASS ZKUF_CL_GENERIC_API IMPLEMENTATION.


  METHOD build_dynamic_where_clause.
    CONSTANTS: lc_and           TYPE string VALUE 'AND',
               lc_equal_symbole TYPE string VALUE '=',
               lc_minus_symbole TYPE string VALUE '-'.

    DATA: lt_api_structure      TYPE zkuf_tt_api_structure,
          lt_relation_key_pairs TYPE zkuf_tt_relation_key_pairs,

          ls_relation_key_pair  TYPE zkuf_st_relation_key_pairs,

          lv_variable           TYPE string,
          lv_where_clause       TYPE string.

    FIELD-SYMBOLS: <fs_api_stucture>      TYPE zkuf_st_api_structure.

    lt_api_structure = it_api_structure.

    LOOP AT lt_api_structure ASSIGNING <fs_api_stucture>.
      lt_relation_key_pairs = <fs_api_stucture>-relationkeys.
      LOOP AT lt_relation_key_pairs INTO ls_relation_key_pair.
        IF <fs_api_stucture>-where_clause IS INITIAL.
          lv_variable = iv_host_variable && lc_minus_symbole && ls_relation_key_pair-parentkey.
          CONCATENATE ls_relation_key_pair-childkey lc_equal_symbole lv_variable
            INTO <fs_api_stucture>-where_clause
            SEPARATED BY space.
        ELSE.
          lv_variable = iv_host_variable && lc_minus_symbole && ls_relation_key_pair-parentkey.
          CONCATENATE ls_relation_key_pair-childkey lc_equal_symbole lv_variable
            INTO lv_where_clause
            SEPARATED BY space.
          CONCATENATE <fs_api_stucture>-where_clause lc_and lv_where_clause
            INTO <fs_api_stucture>-where_clause
            SEPARATED BY space.
          CLEAR: lv_variable, lv_where_clause.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    rt_api_structure = lt_api_structure.
  ENDMETHOD.


  METHOD build_json.
    DATA: lr_table_desc             TYPE REF TO cl_abap_tabledescr,
          lr_struc_desc             TYPE REF TO cl_abap_structdescr,
          lr_composed_struc         TYPE REF TO data,
          lr_composed_tab           TYPE REF TO data,
          lr_struct_comp_name_error TYPE REF TO cx_sy_struct_comp_name,

          lt_comp_tab               TYPE cl_abap_structdescr=>component_table,
          lt_comp_tab_temp          TYPE cl_abap_structdescr=>component_table,

          ls_api_structure          TYPE zkuf_st_api_structure,

          lv_current_depth          TYPE i VALUE 0.

    FIELD-SYMBOLS: <ft_table_ref>     TYPE STANDARD TABLE,
                   <ft_tab_cont>      TYPE STANDARD TABLE,

                   <fs_struc_ref>     TYPE any,
                   <fs_tab_line_cont> TYPE any.
*--------------------------------------------------------------------*
*     Build where clause
*     Name of the host variable for LOOP must be known
*     ( In this case it is "<fs_comp_struc_w_data>" )
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
*     Determine the root Table
*--------------------------------------------------------------------*
    LOOP AT it_api_structure INTO ls_api_structure WHERE depth = 0.
      IF me->mv_root_table_name IS INITIAL.
        me->mv_root_table_name = ls_api_structure-parenttable.
      ELSEIF me->mv_root_table_name <> ls_api_structure-parenttable.
        " Two diferent Root tables not allowed
        " TODO Exception
        RETURN.
      ENDIF.
    ENDLOOP.
*--------------------------------------------------------------------*
*     Create (deep) structure from selected Tables.
*--------------------------------------------------------------------*
    lr_struc_desc ?= cl_abap_structdescr=>describe_by_name( me->mv_root_table_name ).
    lt_comp_tab = lr_struc_desc->get_components( ).

    TRY .
        lt_comp_tab_temp = me->generate_structure(
                       it_api_structure = it_api_structure
                       iv_depth         = lv_current_depth
                       iv_parent        = me->mv_root_table_name
                     ).
      CATCH cx_sy_struct_comp_name INTO lr_struct_comp_name_error.
        RAISE EXCEPTION lr_struct_comp_name_error.
    ENDTRY.

    APPEND LINES OF lt_comp_tab_temp TO lt_comp_tab.

    " Create output table
    CLEAR: lr_struc_desc.
    lr_struc_desc = cl_abap_structdescr=>create( lt_comp_tab ).
    lr_table_desc ?= cl_abap_tabledescr=>create( lr_struc_desc ).
    CREATE DATA lr_composed_tab TYPE HANDLE lr_table_desc.
    ASSIGN lr_composed_tab->* TO <ft_table_ref>.

    " Create structure
    CREATE DATA lr_composed_struc TYPE HANDLE lr_struc_desc.
    ASSIGN lr_composed_struc->* TO <fs_struc_ref>.

*--------------------------------------------------------------------*
*     Fill Table with Data
*--------------------------------------------------------------------*
    CLEAR: ls_api_structure.
    READ TABLE it_api_structure INTO ls_api_structure WITH KEY parenttable = me->mv_root_table_name.
    IF sy-subrc <> 0.
      " No Root
      RETURN.
    ELSEIF ls_api_structure-parent_table_content IS INITIAL.
      " No content for Root
      RETURN.
    ENDIF.

    ASSIGN ls_api_structure-parent_table_content->* TO <ft_tab_cont>.
    LOOP AT <ft_tab_cont> ASSIGNING <fs_tab_line_cont>.
      MOVE-CORRESPONDING <fs_tab_line_cont> TO <fs_struc_ref>.
      me->map_data_to_structure(
        EXPORTING
          it_api_structure    = it_api_structure                 " Table for Hierarchy from psot request
          is_comp_struc       = <fs_struc_ref>
          iv_depth            = 0
        IMPORTING
          es_comp_struc_w_data = <fs_struc_ref>
      ).

      APPEND <fs_struc_ref> TO <ft_table_ref>.
      CLEAR <fs_struc_ref>.
    ENDLOOP.

    /ui2/cl_json=>serialize(
      EXPORTING
        data             = <ft_table_ref>                 " Data to serialize
      RECEIVING
        r_json           = rv_json                 " JSON string
    ).

  ENDMETHOD.


  METHOD constructor.
    DATA: lt_api_key_pair_db TYPE TABLE OF zkuf_d_api_key_p,
          lt_api_structure   TYPE zkuf_tt_api_structure,

          ls_api_key_pair_db TYPE zkuf_d_api_key_p.

    FIELD-SYMBOLS: <fs_api_structure> TYPE zkuf_st_api_structure.
    " Get stored API structure
    SELECT * FROM zkuf_d_api_struc
      WHERE uuid = @iv_api_structure_id
      INTO CORRESPONDING FIELDS OF TABLE @lt_api_structure.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_api_structure ASSIGNING <fs_api_structure>.
      CLEAR: lt_api_key_pair_db.

      SELECT * FROM zkuf_d_api_key_p
        WHERE uuid = @<fs_api_structure>-relationkeyuuid
        INTO TABLE @lt_api_key_pair_db.

      MOVE-CORRESPONDING lt_api_key_pair_db TO <fs_api_structure>-relationkeys.
    ENDLOOP.

    " Get Data for Tables
*    lt_api_structure = me->get_content_of_req_tables( it_api_structure = lt_api_structure ).
    lt_api_structure = me->get_content_of_req_tables(
                         it_api_structure = lt_api_structure
                         iv_root_where    = iv_root_where
                       ).

    " Build Where clause
    me->build_dynamic_where_clause(
      EXPORTING
        it_api_structure             = lt_api_structure
        iv_host_variable             = '<fs_comp_struc_w_data>'
      RECEIVING
        rt_api_structure             = lt_api_structure
    ).

    UNASSIGN <fs_api_structure>.

    " Fill missing Alias
    LOOP AT lt_api_structure ASSIGNING <fs_api_structure>.
      IF <fs_api_structure>-childtablealias IS INITIAL.
        <fs_api_structure>-childtablealias = <fs_api_structure>-childtable.
      ENDIF.
    ENDLOOP.

    me->mt_api_structure = lt_api_structure.

  ENDMETHOD.


  METHOD generate_structure.
*--------------------------------------------------------------------*
* Not working for Root note
* Root must be handled seperatly because Root has no parent
*--------------------------------------------------------------------*
    DATA: lr_table_desc             TYPE REF TO cl_abap_tabledescr,
          lr_struc_desc             TYPE REF TO cl_abap_structdescr,
          lr_struct_comp_name_error TYPE REF TO cx_sy_struct_comp_name,

          lt_comp_tab               TYPE cl_abap_structdescr=>component_table,
          lt_comp_tab_temp          TYPE cl_abap_structdescr=>component_table,

          ls_comp_struc             TYPE cl_abap_structdescr=>component,
          ls_api_structure          TYPE zkuf_st_api_structure.

    LOOP AT it_api_structure INTO ls_api_structure WHERE depth = iv_depth AND parenttable = iv_parent.
      " Get Fields of current Table and store them
      lr_struc_desc ?= cl_abap_structdescr=>describe_by_name( ls_api_structure-childtable ).
      lt_comp_tab = lr_struc_desc->get_components( ).

      " Recusive call to get all Children
      TRY .
          lt_comp_tab_temp = me->generate_structure(
                              EXPORTING
                                it_api_structure       = it_api_structure                 " Table for Hierarchy from psot request
                                iv_depth               = iv_depth + 1
                                iv_parent              = ls_api_structure-childtable
                            ).
        CATCH cx_sy_struct_comp_name INTO lr_struct_comp_name_error.
          RAISE EXCEPTION lr_struct_comp_name_error.
      ENDTRY.

      APPEND LINES OF lt_comp_tab_temp TO lt_comp_tab.

      " Create Refernce of current Table and Children and store to retrun Variable
      CLEAR: lr_struc_desc.
      TRY .
          lr_struc_desc = cl_abap_structdescr=>create( lt_comp_tab ).
          lr_table_desc ?= cl_abap_tabledescr=>create( lr_struc_desc ).
        CATCH cx_sy_struct_comp_name INTO lr_struct_comp_name_error.
          RAISE EXCEPTION lr_struct_comp_name_error.
      ENDTRY.

      lr_struc_desc = cl_abap_structdescr=>create( lt_comp_tab ).
      lr_table_desc ?= cl_abap_tabledescr=>create( lr_struc_desc ).

*      ls_comp_struc-name = ls_payload-totable. "Removed for Alias
      ls_comp_struc-name = ls_api_structure-childtablealias.
      ls_comp_struc-type = lr_table_desc.
      APPEND ls_comp_struc TO rt_api_data_structure.

*------------------------------------------------------------------
* Debugging (shows content)
*------------------------------------------------------------------
***    DATA lr_composed_struc TYPE REF TO data.
***    FIELD-SYMBOLS: <fs_table_ref> TYPE any.
***    CREATE DATA lr_composed_struc TYPE HANDLE lr_table_desc.
***    ASSIGN lr_composed_struc->* TO <fs_table_ref>.
*------------------------------------------------------------------
      CLEAR: ls_comp_struc, ls_api_structure, lt_comp_tab_temp, lt_comp_tab, lr_struc_desc, lr_struc_desc.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_content_of_req_tables.
    DATA: lr_parent_table_ref TYPE REF TO data,
          lr_child_table_ref  TYPE REF TO data,

          lt_api_structure    TYPE zkuf_tt_api_structure.

    FIELD-SYMBOLS: <ft_parent_table> TYPE STANDARD TABLE,
                   <ft_child_table>  TYPE STANDARD TABLE.

    lt_api_structure = it_api_structure.
    " Loop all selected Tabeles
    LOOP AT lt_api_structure ASSIGNING FIELD-SYMBOL(<fs_api_structure>).
      " Create dref with Tablename
      CREATE DATA lr_parent_table_ref TYPE TABLE OF (<fs_api_structure>-parenttable).
      CREATE DATA lr_child_table_ref TYPE TABLE OF (<fs_api_structure>-childtable).
      " Assign to Field-Symbol for Select
      ASSIGN lr_parent_table_ref->* TO <ft_parent_table>.
      ASSIGN lr_child_table_ref->* TO <ft_child_table>.
      " Select data into F.-S.
      IF iv_root_where IS NOT INITIAL AND <fs_api_structure>-depth = 0.
        SELECT * FROM (<fs_api_structure>-parenttable) INTO TABLE <ft_parent_table> WHERE (iv_root_where).
      else.
        SELECT * FROM (<fs_api_structure>-parenttable) INTO TABLE <ft_parent_table>.
      ENDIF.
      SELECT * FROM (<fs_api_structure>-childtable) INTO TABLE <ft_child_table>.
      " Use the dref to store the data
      <fs_api_structure>-parent_table_content = lr_parent_table_ref.
      <fs_api_structure>-child_table_content = lr_child_table_ref.
    ENDLOOP.

    rt_api_structure = lt_api_structure.
  ENDMETHOD.


  METHOD get_json.
    DATA lr_struct_comp_name_error TYPE REF TO cx_sy_struct_comp_name.

    TRY .
        rv_json = me->build_json( it_api_structure = me->mt_api_structure ).
      CATCH cx_sy_struct_comp_name INTO lr_struct_comp_name_error.
        RAISE EXCEPTION lr_struct_comp_name_error.
    ENDTRY.
  ENDMETHOD.


  METHOD map_data_to_structure.
    DATA: lr_comp_tab          TYPE REF TO data,
          lr_comp_struc_w_data TYPE REF TO data,

          ls_api_stucture      TYPE zkuf_st_api_structure.

    FIELD-SYMBOLS: <ft_comp_tab>          TYPE STANDARD TABLE,
                   <ft_current_tab_data>  TYPE STANDARD TABLE,

                   <fs_current_tab_data>  TYPE any,
                   <fs_comp_struc>        TYPE any,
                   <fs_comp_struc_w_data> TYPE any.

    " Create lokal and changeable copie of import data
    CREATE DATA lr_comp_struc_w_data LIKE is_comp_struc.
    ASSIGN lr_comp_struc_w_data->* TO <fs_comp_struc_w_data>.
    <fs_comp_struc_w_data> = is_comp_struc.

    " Get all subnotes
    LOOP AT it_api_structure INTO ls_api_stucture WHERE depth = iv_depth.
      " Get store content of subnotes
      ASSIGN COMPONENT ls_api_stucture-childtablealias OF STRUCTURE <fs_comp_struc_w_data> TO <ft_comp_tab>.
      IF sy-subrc = 0.
        ASSIGN ls_api_stucture-child_table_content->* TO <ft_current_tab_data>.
        IF sy-subrc = 0.
          " Get structure of current table
          CREATE DATA lr_comp_tab LIKE LINE OF <ft_comp_tab>.
          ASSIGN lr_comp_tab->* TO <fs_comp_struc>.
          " Move data to output structure
          LOOP AT <ft_current_tab_data> ASSIGNING <fs_current_tab_data> WHERE (ls_api_stucture-where_clause).
            MOVE-CORRESPONDING <fs_current_tab_data> TO <fs_comp_struc>.
            " Recursive call
            me->map_data_to_structure(
              EXPORTING
                it_api_structure          = it_api_structure
                is_comp_struc             = <fs_comp_struc>
                iv_depth                  = iv_depth + 1
              IMPORTING
                es_comp_struc_w_data = <fs_comp_struc>
            ).

            APPEND <fs_comp_struc> TO <ft_comp_tab>.
            CLEAR <fs_comp_struc>.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

    es_comp_struc_w_data = <fs_comp_struc_w_data>.
  ENDMETHOD.
ENDCLASS.
