class ZCL_BP definition
  public
  create public .

public section.

  class-methods GET_PARTNER_TYPE
    importing
      !IV_PARVW type PARVW
    returning
      value(RV_NRART) type NRART .
  class-methods CHECK_SALES_AREA
    importing
      !IV_BU_PARTNER type BU_PARTNER
      !IV_VKORG type VKORG
      !IV_VTWEG type VTWEG
      !IV_SPART type SPART
    returning
      value(RV_VALID) type ABAP_BOOL .
  class-methods CHECK_COMPANY_CODE
    importing
      !IV_BU_PARTNER type BU_PARTNER
      !IV_BUKRS type BUKRS
    returning
      value(RV_VALID) type ABAP_BOOL .
  class-methods CREATE_SHIP_TO_PARTY
    importing
      !IS_ADDRESS type BAPIADDR1
      !IS_PARTNER_DATA type BAPIPARNR
      !IS_ORDER_HEADER type BAPISDHD1 optional
    returning
      value(RV_BU_PARTNER) type BU_PARTNER
    raising
      ZCX_BC_BP .
  class-methods CREATE_SHIP_TO_PARTY_WITH_FM
    importing
      !IS_ADDRESS type BAPIADDR1
      !IS_NOC_PARTNER_DATA type BAPIPARNR
      !IS_ORDER_HEADER type BAPISDHD1 optional
    returning
      value(RV_BU_PARTNER) type BU_PARTNER .
  class-methods CREATE_SHIP_TO_PARTY_SALESAREA
    importing
      !IS_NOC_PARTNER_DATA type BAPIPARNR
      !IV_NOC_VKORG type VKORG
      !IV_NOC_VTWEG type VTWEG
      !IV_NOC_SPART type SPART
      !IV_BEU_VKORG type VKORG
      !IV_BEU_VTWEG type VTWEG
      !IV_BEU_SPART type SPART
      !IV_BEU_PARTNER_NUMB type BU_PARTNER
    raising
      ZCX_BC_BP_SALES_AREA .
  PROTECTED SECTION.
private section.

  class-methods BOOK_BUSINESS_PARTNER
    importing
      !IX_DATA type CVIS_EI_EXTERN
    returning
      value(RV_BU_PARTNER) type BU_PARTNER
    raising
      ZCX_BC_BP .
  class-methods IS_DELETED
    importing
      !IV_BU_PARTNER type BU_PARTNER
    returning
      value(RV_DELETED) type BU_XDELE .
ENDCLASS.



CLASS ZCL_BP IMPLEMENTATION.


  METHOD book_business_partner.

    DATA ls_return TYPE bapiretc.
    DATA ls_retval TYPE mdg_bs_bp_msgmap.

    "validate data
    cl_md_bp_maintain=>validate_single(
    EXPORTING
      i_data            = ix_data
    IMPORTING
      et_return_map     = DATA(lt_return_map)  ).

    "Check for errors
    READ TABLE lt_return_map INTO ls_retval WITH KEY type = 'A'.
    IF sy-subrc > 0.
      READ TABLE lt_return_map INTO ls_retval WITH KEY type = 'E'.
    ENDIF.
    IF sy-subrc = 0.
      "validation failed
      RAISE EXCEPTION TYPE zcx_bc_bp
        EXPORTING
          textid    = VALUE scx_t100key(
               msgid = ls_retval-id
               msgno = ls_retval-number
               attr1 = ls_retval-message_v1
               attr2 = ls_retval-message_v2
               attr3 = ls_retval-message_v3
               attr4 = ls_retval-message_v4 )
          it_return = CORRESPONDING #( lt_return_map ).
    ELSE.
      "no errors came up: create customer ship-to
      SET UPDATE TASK LOCAL.
      cl_md_bp_maintain=>maintain(
        EXPORTING
          i_data     = VALUE #( ( ix_data ) )
        IMPORTING
          e_return   = DATA(lt_return)   ).

      "error messages occurred?
      IF lt_return IS INITIAL.
        sy-subrc = 1.
      ELSE.
        READ TABLE lt_return[ 1 ]-object_msg INTO ls_return WITH KEY type = 'A'.
        IF sy-subrc > 0.
          READ TABLE lt_return[ 1 ]-object_msg INTO ls_return WITH KEY type = 'E'.
        ENDIF.
      ENDIF.

      IF sy-subrc <> 0.

        "commit
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        "get number of created ship-to
        IMPORT lv_partner TO rv_bu_partner FROM MEMORY ID 'BUP_MEMORY_PARTNER'.
      ELSE.
        "update failed although validation was ok... :/
        RAISE EXCEPTION TYPE zcx_bc_bp
          EXPORTING
            textid    = VALUE scx_t100key(
                 msgid = ls_return-id
                 msgno = ls_return-number
                 attr1 = ls_return-message_v1
                 attr2 = ls_return-message_v2
                 attr3 = ls_return-message_v3
                 attr4 = ls_return-message_v4 )
            it_return = CORRESPONDING #( VALUE #( lt_return[ 1 ]-object_msg OPTIONAL ) ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_company_code.

    SELECT SINGLE 'X' AS valid
      FROM lfb1
      INTO @rv_valid
     WHERE lifnr = @iv_bu_partner
       AND bukrs = @iv_bukrs.

  ENDMETHOD.


  METHOD check_sales_area.

*    SELECT SINGLE 'X'
*      INTO @rv_valid
*      FROM knvv
*     WHERE kunnr = @iv_bu_partner
*       AND vkorg = @iv_vkorg
*       AND vtweg = @iv_vtweg
*       AND spart = @iv_spart.

    DATA lt_sales_areas TYPE cvis_sales_area_info_t.

    DATA lo_so_sales TYPE REF TO cvi_so_customer_sales.
    lo_so_sales ?= fsbp_segment_factory=>get_instance(
       i_partner = iv_bu_partner
       i_name    = if_cvi_const_xo_objects_cust=>so_sales ).
    DATA(lo_sales_area) = lo_so_sales->get_sales_areas( ).
    lt_sales_areas = lo_sales_area->get_sales_area_info( ).

    IF line_exists(
      lt_sales_areas[
        sales_org    = iv_vkorg
        dist_channel = iv_vtweg
        division     = iv_spart ] ).
      rv_valid = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD create_ship_to_party.

    "copy given address data...
    DATA(ls_address) = is_address.

    "because we have to remove location specific fields
    ls_address-build_long = space. "building
    ls_address-building   = space. "building
    ls_address-floor      = space. "floor
    ls_address-room_no    = space. "room number
    ls_address-c_o_name   = space. "department
    ls_address-adr_notes  = space. "remark

    "create unique id for customer creation
    TRY.
        DATA(lv_guid) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        RETURN.
    ENDTRY.

    "fill data structure with relevant customer data
    DATA(lx_data) = VALUE cvis_ei_extern(
      partner-header-object_task     = 'I'
      partner-header-object          = 'BUS1006' "Business Partner
      partner-header-object_instance = VALUE #(
        bpartnerguid           = lv_guid )
        partner-central_data-common = VALUE #(
          data = VALUE #(
            bp_control      = VALUE #(
                                category = '2' ) "organization
*                                grouping = 'SHIP' ) "ship-to
            bp_organization = VALUE #(
                                name1 = ls_address-name
                                name2 = ls_address-name_2
                                name3 = ls_address-name_3
                                name4 = ls_address-name_4 )
            bp_centraldata  = VALUE #(
                                searchterm1 = ls_address-sort1
                                searchterm2 = ls_address-sort2
                                title_key   = '0003'  "company
*                                partnertype = '0001'
                                 )
                       ) )
        partner-central_data-role-roles = VALUE #( task = 'I'
          ( data_key = 'FLCU01'
            data     = VALUE #(
                       rolecategory = 'FLCU01'
                            ) ) )

        partner-central_data-address = VALUE #(
          current_state = 'C'
          time_dependent = abap_false
          addresses = VALUE #( (
             task               = 'I'
             data_key-operation = 'I'
             currently_valid    = abap_true
             data               = VALUE #(
                  postal = VALUE #(
                      data = VALUE #( BASE CORRESPONDING #( ls_address ) standardaddress = 'X' )
                                      )
                                     )
                                    )
                                   )
                                  )
      customer-header-object_instance-kunnr = space "rv_bu_partner
      customer-header-object_task   = 'I'
      ensure_create-create_customer = abap_true
      customer-sales_data-current_state = 'C'
      customer-sales_data-sales     = VALUE #(
                                      ( task = 'I'
                                        data_key-vkorg      = is_order_header-sales_org
                                        data_key-vtweg      = is_order_header-distr_chan
                                        data_key-spart      = is_order_header-division
                                        data-waers          = 'EUR'
                                        data-lprio          = '2'  "Delivery priority
                                        data-vsbed          = '01' "shipping conditions
                                        data-antlf          = '9'  "maximum number of permitted part deliveries per item
                                        )
                                 )
                             ).

    rv_bu_partner = book_business_partner( lx_data ).

  ENDMETHOD.


  METHOD create_ship_to_party_salesarea.


    "BP GUID necessary for updating partner
    SELECT SINGLE partner_guid FROM but000 INTO @DATA(lv_guid) WHERE partner = @iv_beu_partner_numb.

    "check if there are sales areas. if yes: Update data, if no: Insert data
    "Task "M" is not supported for Customers... :(
    SELECT COUNT( * ) FROM knvv
      INTO @DATA(lv_knvv_count)
     WHERE kunnr = @iv_beu_partner_numb.
    IF lv_knvv_count = 0.
      DATA(header_task) = 'I'.
    ELSE.
      header_task = 'U'.
    ENDIF.


    "fill header data, role and sales area
    DATA(lx_data) = VALUE cvis_ei_extern(
      partner-header-object_task     = 'M'
      partner-header-object_instance = VALUE #(
                        bpartner     = iv_beu_partner_numb
                        bpartnerguid = lv_guid )
      partner-central_data-role-roles = VALUE #( (
        task     = 'I'
        data_key = 'FLCU01'
        data     = VALUE #( rolecategory = 'FLCU01' ) ) )
      customer-header-object_instance-kunnr = iv_beu_partner_numb
      customer-header-object_task = header_task
      customer-sales_data-sales = VALUE #( (
         task                = 'I'
         data_key-vkorg      = iv_beu_vkorg
         data_key-vtweg      = iv_beu_vtweg
         data_key-spart      = iv_beu_spart
         data-lprio          = '2'  "Delivery priority
         data-vsbed          = '01' "shipping conditions
         data-antlf          = '9'  "maximum number of permitted part deliveries per item
         ) ) ).

    "validate data
    cl_md_bp_maintain=>validate_single(
    EXPORTING
      i_data                   = lx_data
    IMPORTING
      et_return_map            = DATA(lt_return_map)  ).
    IF line_exists( lt_return_map[ type = 'E' ] ).
      "error creating sales area
      RAISE EXCEPTION TYPE zcx_bc_bp_sales_area
        EXPORTING
          it_return = CORRESPONDING #( lt_return_map ).
    ELSE.

      "no errors came up: create customer ship-to
      cl_md_bp_maintain=>maintain(
        EXPORTING
          i_data     = VALUE #( ( lx_data ) )
        IMPORTING
          e_return   = DATA(ls_return)   ).
      IF line_exists( ls_return[ 1 ]-object_msg[ type = 'E' ] )
      OR line_exists( ls_return[ 1 ]-object_msg[ type = 'A' ] ).
        "error creating sales area
        RAISE EXCEPTION TYPE zcx_bc_bp_sales_area
          EXPORTING
            it_return = CORRESPONDING #( ls_return[ 1 ]-object_msg ).
      ELSE.
        "commit
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD create_ship_to_party_with_fm.

    DATA ls_message      TYPE bapiret2.
    DATA ls_central_data TYPE bapibus1006_central.
    DATA ls_person_data  TYPE bapibus1006_central_person.
    DATA ls_organi_data  TYPE bapibus1006_central_organ.
    DATA ls_address      TYPE bapibus1006_address.
    DATA lt_messages     TYPE bapiret2_t.

    "assure synchronous posting of partner
    SET UPDATE TASK LOCAL.

    "set data
    ls_central_data-searchterm1 = to_upper( is_address-name ).
    ls_central_data-title_key   = '0003'. "Company.

*    ls_organi_data = CORRESPONDING #( is_address ).

    ls_organi_data-name1 = is_address-name.
    ls_organi_data-name2 = is_address-name_2.
    ls_organi_data-name3 = is_address-name_3.
    ls_organi_data-name4 = is_address-name_4.

    ls_address = CORRESPONDING #( is_address ).
    ls_address-standardaddress  = 'X'.

    CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
      EXPORTING
        partnergroup            = 'SHIP'
        partnercategory         = '2' "organisation
        centraldata             = ls_central_data
        centraldataorganization = ls_organi_data
        addressdata             = ls_address
      IMPORTING
        businesspartner         = rv_bu_partner
      TABLES
        return                  = lt_messages.

    CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
      EXPORTING
        businesspartner             = rv_bu_partner
        businesspartnerrolecategory = 'FLCU01'
      TABLES
        return                      = lt_messages.

    IF lt_messages  IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ELSE.
      CLEAR rv_bu_partner.
    ENDIF.


  ENDMETHOD.


  METHOD get_partner_type.

    "get partner type
    CALL FUNCTION 'MM_PARTNER_TYPE'
      EXPORTING
        i_parvw                   = iv_parvw
      IMPORTING
        e_nrart                   = rv_nrart
      EXCEPTIONS
        partnerfunction_not_found = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      rv_nrart = space.
    ENDIF.

  ENDMETHOD.


  METHOD is_deleted.

    SELECT SINGLE xdele
      FROM but000 INTO rv_deleted
     WHERE partner = iv_bu_partner.

  ENDMETHOD.
ENDCLASS.
