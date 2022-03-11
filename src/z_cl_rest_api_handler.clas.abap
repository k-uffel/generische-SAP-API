CLASS z_cl_rest_api_handler DEFINITION
  PUBLIC

  INHERITING FROM cl_rest_http_handler
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF object_parameter_s,
        instance TYPE REF TO object,
      END OF object_parameter_s .
    TYPES:
      BEGIN OF parameter_s,
        name  TYPE string,
        value TYPE REF TO data,
      END OF parameter_s .
    TYPES:
      parameter_t TYPE HASHED TABLE OF parameter_s WITH UNIQUE KEY name .

    METHODS: if_rest_application~get_root_handler REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z_CL_REST_API_HANDLER IMPLEMENTATION.


  METHOD if_rest_application~get_root_handler.
    DATA(lr_router) = NEW cl_rest_router( ).

    lr_router->attach( iv_template = '/content'
                       iv_handler_class = 'Z_CL_REST_API_RESOURCE' "OLD!!!
                       ).

    lr_router->attach( iv_template = '/genericapi'
                   iv_handler_class = 'ZKUF_CL_GENERIC_API_RES'
                   ).

    ro_root_handler = lr_router.
  ENDMETHOD.
ENDCLASS.
