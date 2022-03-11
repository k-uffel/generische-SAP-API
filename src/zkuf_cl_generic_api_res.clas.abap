class ZKUF_CL_GENERIC_API_RES definition
  public
  inheriting from CL_REST_RESOURCE
  create public .

public section.

  methods IF_REST_RESOURCE~POST
    redefinition .
  methods IF_REST_RESOURCE~GET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZKUF_CL_GENERIC_API_RES IMPLEMENTATION.


  METHOD if_rest_resource~get.
    CONSTANTS: lc_and                TYPE string VALUE 'AND',
               lc_like               TYPE string VALUE 'LIKE',
               lc_equal_symbole      TYPE string VALUE '=',
               lc_apostrophe_symbole TYPE string VALUE ''''.

    DATA: lt_parameters             TYPE tihttpnvp,
          lt_query_parameters       TYPE tihttpnvp,

          ls_parameter              TYPE ihttpnvp,
          ls_query_parameter        TYPE ihttpnvp,

          lv_json                   TYPE string,
          lv_root_where             TYPE string,
          lv_stringified_value      TYPE string,

          lr_struct_comp_name_error TYPE REF TO cx_sy_struct_comp_name,
          lr_generic_api            TYPE REF TO zkuf_cl_generic_api.

    lt_parameters = mo_request->get_uri_query_parameters( ).

    READ TABLE lt_parameters WITH KEY name = 'auth' INTO ls_parameter.
    IF sy-subrc = 0 AND ls_parameter-value = 'X'.
      " Just get Status 200 and CSRF-Token
      RETURN.
    ENDIF.

    LOOP AT lt_parameters INTO ls_query_parameter WHERE name <> 'api_uuid'.
      IF ls_query_parameter-value is INITIAL or ls_query_parameter-value CS 'undefined'.
        CONTINUE.
      ENDIF.
      " Convert parameter Value into String for WHERE of SELECT. Value must be enclosed by apostrophes
      CONCATENATE lc_apostrophe_symbole ls_query_parameter-value lc_apostrophe_symbole
        INTO lv_stringified_value.

      IF lv_stringified_value CS '%25'.
        REPLACE ALL OCCURRENCES OF '%25' IN lv_stringified_value WITH '%'.
        IF lv_root_where IS INITIAL.
          CONCATENATE ls_query_parameter-name lc_like lv_stringified_value
            INTO lv_root_where
            SEPARATED BY space.
        ELSE.
          CONCATENATE lv_root_where lc_and ls_query_parameter-name lc_like lv_stringified_value
            INTO lv_root_where
            SEPARATED BY space.
        ENDIF.
      ELSE.
        IF lv_root_where IS INITIAL.
          CONCATENATE ls_query_parameter-name lc_equal_symbole lv_stringified_value
            INTO lv_root_where
            SEPARATED BY space.
        ELSE.
          CONCATENATE lv_root_where lc_and ls_query_parameter-name lc_equal_symbole lv_stringified_value
            INTO lv_root_where
            SEPARATED BY space.
        ENDIF.
      ENDIF.

      CLEAR: lv_stringified_value.
    ENDLOOP.

    READ TABLE lt_parameters WITH KEY name = 'api_uuid' INTO ls_parameter.
    IF sy-subrc = 0.
      CREATE OBJECT lr_generic_api
        EXPORTING
          iv_api_structure_id = CONV #( ls_parameter-value ) " 16 Byte UUID in 22 Characters (Usually Base64 Encoded)
          iv_root_where       = lv_root_where.

      DATA(lr_response) = mo_response->create_entity( ).

      TRY .
          lv_json = lr_generic_api->get_json( ).
          lr_response->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).
          lr_response->set_string_data( iv_data = lv_json ).
        CATCH cx_sy_struct_comp_name INTO lr_struct_comp_name_error.
          mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
          lr_response->set_content_type( iv_media_type = if_rest_media_type=>gc_text_plain ).
          lr_response->set_string_data( iv_data = lr_struct_comp_name_error->get_text( ) ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD if_rest_resource~post.
    DATA: lt_api_structures         TYPE zkuf_tt_api_structure,
          lt_api_struc_db           TYPE TABLE OF zkuf_d_api_struc,
          lt_api_key_pairs_db       TYPE TABLE OF zkuf_d_api_key_p,

          ls_api_structure          TYPE zkuf_st_api_structure,
          ls_api_key_pair           TYPE zkuf_st_relation_key_pairs,

          lv_json_data              TYPE string,
          lv_response_json          TYPE string,
          lv_uuid_relation_key      TYPE sysuuid_c22,
          lv_uuid_api_struc         TYPE sysuuid_c22,

          lr_struct_comp_name_error TYPE REF TO cx_sy_struct_comp_name,
          lr_generate_uuid          TYPE REF TO if_system_uuid.

    io_entity->get_content_type(
      IMPORTING
        ev_media_type = DATA(lv_media_type)                 " Media Type
    ).

    IF lv_media_type <> if_rest_media_type=>gc_appl_json.
      mo_response->set_status( cl_rest_status_code=>gc_client_error_unsup_media_ty ).
      RETURN.
    ENDIF.

    lv_json_data = io_entity->get_string_data( ).

    IF lv_json_data IS INITIAL.
      mo_response->set_status( cl_rest_status_code=>gc_client_error_bad_request ).
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = lv_json_data                 " JSON string
      CHANGING
        data             = lt_api_structures                 " Data to serialize
    ).

    lr_generate_uuid = cl_uuid_factory=>create_system_uuid( ).

    TRY .
        lv_uuid_api_struc = lr_generate_uuid->create_uuid_c22( ).
      CATCH cx_uuid_error.
        " unable to get uuid
    ENDTRY.

    LOOP AT lt_api_structures INTO ls_api_structure.
      TRY .
          lv_uuid_relation_key = lr_generate_uuid->create_uuid_c22( ).
        CATCH cx_uuid_error.
          " unable to get uuid
      ENDTRY.
      LOOP AT ls_api_structure-relationkeys INTO ls_api_key_pair.
        lt_api_key_pairs_db = VALUE #( BASE lt_api_key_pairs_db ( mandt = sy-mandt
                                                                  uuid = lv_uuid_relation_key
                                                                  parentkey = ls_api_key_pair-parentkey
                                                                  childkey = ls_api_key_pair-childkey ) ).
      ENDLOOP.

      lt_api_struc_db = VALUE #( BASE lt_api_struc_db ( mandt = sy-mandt
                                                        uuid = lv_uuid_api_struc
                                                        depth = ls_api_structure-depth
                                                        parenttable = ls_api_structure-parenttable
                                                        childtable = ls_api_structure-childtable
                                                        childtablealias = ls_api_structure-childtablealias
                                                        relationkeyuuid = lv_uuid_relation_key ) ).


    ENDLOOP.

    MODIFY zkuf_d_api_struc FROM TABLE lt_api_struc_db.
    IF sy-subrc <> 0.
      mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
      mo_response->set_reason( iv_reason = 'Database update Failed (zkuf_d_api_struc)' ).
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    MODIFY zkuf_d_api_key_p FROM TABLE lt_api_key_pairs_db.
    IF sy-subrc <> 0.
      mo_response->set_status( cl_rest_status_code=>gc_server_error_internal ).
      mo_response->set_reason( iv_reason = 'Database update Failed (zkuf_d_api_key_p)' ).
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    mo_response->set_header_field(
      EXPORTING
        iv_name  = 'api_id'
        iv_value = CONV #( lv_uuid_api_struc )
    ).

  ENDMETHOD.
ENDCLASS.
