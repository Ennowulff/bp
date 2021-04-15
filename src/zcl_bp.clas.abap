class ZCL_BP definition
  public
  create public .

public section.

  class-methods LOCK
    importing
      !IV_BU_PARTNER type BU_PARTNER
    raising
      ZCX_BC_BP_LOCKED .
  class-methods UNLOCK
    importing
      !IV_BU_PARTNER type BU_PARTNER .
  methods CONSTRUCTOR
    importing
      !IV_PARVW type PARVW
      !IV_NUMBER type BU_PARTNER optional
      !IV_ID type BU_ID_NUMBER optional .
  class-methods FIND_CUSTOMER_BY_ID
    importing
      !IV_EXT_ID type BU_ID_NUMBER
    returning
      value(RV_BU_PARTNER) type BU_PARTNER .
  class-methods FIND_VENDOR_BY_ID
    importing
      !IV_EXT_ID type BU_ID_NUMBER
    returning
      value(RV_BU_PARTNER) type BU_PARTNER .
  class-methods GET_CUSTOMER_ID
    importing
      !IV_BU_PARTNER type BU_PARTNER
    returning
      value(RV_EXT_ID) type BU_ID_NUMBER .
  class-methods GET_VENDOR_ID
    importing
      !IV_BU_PARTNER type BU_PARTNER
    returning
      value(RV_EXT_ID) type BU_ID_NUMBER .
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
  class-methods FIND_CUSTOMER_IN_S_AREA_BY_ID
    importing
      !IV_EXT_ID type BU_ID_NUMBER
      !IV_VKORG type VKORG
      !IV_VTWEG type VTWEG
      !IV_SPART type SPART
    returning
      value(RV_BU_PARTNER) type BU_PARTNER
    raising
      ZCX_BC_BP_SALES_AREA
      ZCX_BC_BP .
  class-methods FIND_VENDOR_IN_COMP_CODE_BY_ID
    importing
      !IV_EXT_ID type BU_ID_NUMBER
      !IV_BUKRS type BUKRS optional
      !IV_SALES_ORG type VKORG optional
    returning
      value(RV_BU_PARTNER) type BU_PARTNER .
  class-methods CREATE_SHIP_TO_PARTY
    importing
      !IS_ADDRESS type BAPIADDR1
      !IS_NOC_PARTNER_DATA type BAPIPARNR
      !IS_ORDER_HEADER type BAPISDHD1 optional
      !IV_NOC_VKORG type VKORG
      !IV_NOC_VTWEG type VTWEG
      !IV_NOC_SPART type SPART
    returning
      value(RV_BU_PARTNER) type BU_PARTNER
    raising
      ZCX_BC_BP .
  class-methods CREATE_SHIP_TO_PARTY_WITH_FM
    importing
      !IS_ADDRESS type BAPIADDR1
      !IS_NOC_PARTNER_DATA type BAPIPARNR
      !IS_ORDER_HEADER type BAPISDHD1 optional
      !IV_NOC_VKORG type VKORG
      !IV_NOC_VTWEG type VTWEG
      !IV_NOC_SPART type SPART
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
  class-methods CREATE_SHIP_TO_PARTY_ZKLI
    importing
      !IS_ADDRESS type BAPIADDR1
      !IS_NOC_PARTNER_DATA type BAPIPARNR
      !IS_ORDER_HEADER type BAPISDHD1 optional
      !IV_NOC_VKORG type VKORG
      !IV_NOC_VTWEG type VTWEG
      !IV_NOC_SPART type SPART
    returning
      value(RV_BU_PARTNER) type BU_PARTNER
    raising
      ZCX_BC_BP .
  class-methods CREATE_APO_LOCATION
    importing
      !IV_BU_PARTNER type BU_PARTNER .
  PROTECTED SECTION.
PRIVATE SECTION.

  CLASS-METHODS book_business_partner
    IMPORTING
      !ix_data TYPE cvis_ei_extern
    RETURNING
      VALUE(rv_bu_partner) TYPE bu_partner
    RAISING
      zcx_bc_bp .
  CLASS-METHODS get_related_beu_sales_org
    IMPORTING
      !iv_noc_vkorg TYPE vkorg
    RETURNING
      VALUE(rv_beu_vkorg) TYPE vkorg
    RAISING
      zcx_bc_bp .
  CLASS-METHODS is_deleted
    IMPORTING
      !iv_bu_partner TYPE bu_partner
    RETURNING
      VALUE(rv_deleted) TYPE bu_xdele .
ENDCLASS.



CLASS ZCL_BP IMPLEMENTATION.


  METHOD book_business_partner.

    DATA ls_return TYPE bapiretc.
    DATA ls_retval TYPE mdg_bs_bp_msgmap.

    "get noc partner number from data
    TRY.
        DATA(lv_noc_partner) = CONV bu_partner( ix_data-partner-central_data-ident_number-ident_numbers[ 1 ]-data_key-identificationnumber ).
      CATCH cx_sy_itab_line_not_found ##no_handler.
    ENDTRY.


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
        sy-subrc = 7.
      ELSE.
        READ TABLE lt_return[ 1 ]-object_msg INTO ls_return WITH KEY type = 'A'.
        IF sy-subrc > 0.
          READ TABLE lt_return[ 1 ]-object_msg INTO ls_return WITH KEY type = 'E'.
        ENDIF.
      ENDIF.

      IF sy-subrc = 0.
        "unlock before raising error
        unlock( lv_noc_partner ).

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
            it_return = CORRESPONDING #( lt_return[ 1 ]-object_msg ).
      ELSE.
        "commit
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        "get number of created ship-to
        IMPORT lv_partner TO rv_bu_partner FROM MEMORY ID 'BUP_MEMORY_PARTNER'.
      ENDIF.

    ENDIF.

    "Unlock NOC partner number
    IF lv_noc_partner IS NOT INITIAL.
      unlock( lv_noc_partner ).
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


  METHOD constructor.
  ENDMETHOD.


  METHOD create_apo_location.

*    /sapapo/cl_loc_process=>create_locations_from_report(
*      EXPORTING
*        it_bpnumb = VALUE #( ( sign = 'I' option = 'EQ' low = iv_bu_partner ) )
*      IMPORTING
*        et_loc    = DATA(lt_locations) ).

  ENDMETHOD.


  METHOD create_ship_to_party.

    "copy given address data...
    DATA(ls_address) = is_address.

    "Due to parallel processing issues we need to "lock" the not-yet existing partner
    "to prevent other processes to simultaneously create the same ship-to
*    lock( is_noc_partner_data-partn_numb ).

    "because we have to remove location specific fields
    ls_address-build_long = space. "building
    ls_address-building   = space. "building
    ls_address-floor      = space. "floor
    ls_address-room_no    = space. "room number
    ls_address-c_o_name   = space. "department
    ls_address-adr_notes  = space. "remark

    "read ECC customer via rfc
*    DATA(ls_ecc_customer) = zcl_core3_helper=>get_ecc_customer(
*      iv_kunnr = is_noc_partner_data-partn_numb
*      iv_vkorg = iv_noc_vkorg
*      iv_vtweg = iv_noc_vtweg
*      iv_spart = iv_noc_spart ).

*    IF ls_ecc_customer-inco1(1) = 'Z'.
*      CLEAR ls_ecc_customer-inco1.
*      CLEAR ls_ecc_customer-inco2.
*    ENDIF.

*    IF ls_address-sort1 IS INITIAL.
*      ls_address-sort1 = ls_ecc_customer-sortl.
*    ENDIF.

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
                                 ) "ship-to
                       ) )
        partner-central_data-role-roles = VALUE #( task = 'I'
          ( data_key = 'FLCU01'
            data     = VALUE #(
                       rolecategory = 'FLCU01'
                            ) ) )
*       partner-central_data-ident_number  = VALUE #( ident_numbers = VALUE #(
*       ( task     = 'I'
*         data_key = VALUE #( identificationcategory = 'ZCUST'
*                             identificationnumber   = |{ is_noc_partner_data-partn_numb ALPHA = OUT }| )
*         data     = VALUE #( idinstitute            = |{ is_order_header-sales_org }/{ is_order_header-distr_chan }/{ is_order_header-division }| ) ) ) )

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
                                    remark = VALUE #(
                                      current_state = 'C'
                                      remarks = VALUE #(
                                                ( task = 'I'
                                                  data = VALUE #(
                                                           langu     = 'E'
                                                           langu_iso = 'EN'
                                                           adr_notes = 'automatically created'(aen) ) )
                                                ( task = 'I'
                                                  data = VALUE #(
                                                           langu     = 'D'
                                                           langu_iso = 'DE'
                                                           adr_notes = 'Automatisch angelegt'(ade) ) )
                                                   )
                                                   )
                                    addr_usage-current_state = 'C'
                                    addr_usage-addr_usages   = VALUE #(
                                                              ( task                 = 'I'
                                                                currently_valid      = 'X'
                                                                data_key-addresstype = 'XXDEFAULT' "'FIRMA' " '0003' "business address
                                                                data-standard        = abap_true )
                                                              ( task                 = 'I'
                                                                currently_valid      = 'X'
                                                                data_key-addresstype = 'FIRMA' " '0003' "business address
                                                                data-standard        = abap_true )
                                                                       )
                                     )
                                    )
                                   )
                                  )
      customer-header-object_instance-kunnr = space "rv_bu_partner
      customer-header-object_task   = 'I'
      ensure_create-create_customer = abap_true
      customer-sales_data-sales     = VALUE #(
                                      ( task = 'I'
                                        data_key-vkorg      = is_order_header-sales_org
                                        data_key-vtweg      = is_order_header-distr_chan
                                        data_key-spart      = is_order_header-division
                                        data-waers          = 'EUR'
                                        data-lprio          = '2'  "Delivery priority
                                        data-vsbed          = '01' "shipping conditions
                                        data-antlf          = '9'  "maximum number of permitted part deliveries per item
*                                        data-z_batch_determ = ls_ecc_customer-kvgr4
*                                        data-inco1          = ls_ecc_customer-inco1
*                                        data-inco2          = ls_ecc_customer-inco2
                                        )
                                        )
                             ).

    rv_bu_partner = book_business_partner( lx_data ).

  ENDMETHOD.


  METHOD create_ship_to_party_salesarea.



*    "maintain sales area view manually!
*    "as far as thec creation of the sales area does not work properly...
*    RAISE EXCEPTION TYPE zcx_bc_bp_sales_area
*      EXPORTING
*        it_return = VALUE #( ( id = 'ZCORE' number = '080' type = 'E' message_v1 = iv_beu_partner_numb ) ).

    "read ECC customer via rfc
*    DATA(ls_ecc_customer) = zcl_core3_helper=>get_ecc_customer(
*      iv_kunnr = is_noc_partner_data-partn_numb
*      iv_vkorg = iv_noc_vkorg
*      iv_vtweg = iv_noc_vtweg
*      iv_spart = iv_noc_spart ).

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
*         data-z_batch_determ = ls_ecc_customer-kvgr4
*         data-inco1          = ls_ecc_customer-inco1
*         data-inco2          = ls_ecc_customer-inco2
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


  METHOD create_ship_to_party_zkli.

    "special routine for ZKLI
    "- account group: KM91
    "- grouping: TECH instead of SHIP


    "Due to parallel processing issues we need to "lock" the not-yet existing partner
    "to prevent other processes to simultaneously create the same ship-to
    lock( is_noc_partner_data-partn_numb ).

    "copy given address data...
    DATA(ls_address) = is_address.

    "because we have to remove location specific fields
    ls_address-build_long = space. "building
    ls_address-building   = space. "building
    ls_address-floor      = space. "floor
    ls_address-room_no    = space. "room number
    ls_address-c_o_name   = space. "department
    ls_address-adr_notes  = space. "remark

    "language
    IF ls_address-langu IS INITIAL.
      "read country specific language
      SELECT SINGLE spras
        FROM t005 INTO ls_address-langu
       WHERE land1 = is_address-country.
      "read language ISO code for language
      SELECT SINGLE laiso
        FROM t002 INTO ls_address-langu_iso
       WHERE spras = ls_address-langu.
    ENDIF.

    "read ECC customer via rfc
*    DATA(ls_ecc_customer) = zcl_core3_helper=>get_ecc_customer(
*      iv_kunnr = is_noc_partner_data-partn_numb
*      iv_vkorg = iv_noc_vkorg
*      iv_vtweg = iv_noc_vtweg
*      iv_spart = iv_noc_spart ).
*
*    IF ls_ecc_customer-inco1(1) = 'Z'.
*      CLEAR ls_ecc_customer-inco1.
*      CLEAR ls_ecc_customer-inco2.
*    ENDIF.
*
*    IF ls_address-sort1 IS INITIAL.
*      ls_address-sort1 = ls_ecc_customer-sortl.
*    ENDIF.

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
                                category = '1'      "person
                                grouping = 'TECH' ) "ship-to ZKLI (Technician)
            bp_person       = VALUE #(
              correspondlanguage = is_address-langu
              firstname          = is_address-name
              lastname           = is_address-name_2 )
            bp_centraldata  = VALUE #(
                                searchterm1 = ls_address-sort1
                                searchterm2 = ls_address-sort2
                                title_key   = ''  "Mr./Mrs.? Don't know
                                partnertype = '0020' ) "ship-to "0003 employee??
                       ) )
        partner-central_data-role-roles = VALUE #(
          ( data_key = 'FLCU01'
            data     = VALUE #(  rolecategory = 'FLCU01'  ) )
          ( data_key = 'FLCU02'
            data     = VALUE #(  rolecategory = 'FLCU02'  ) )
         )
        partner-central_data-ident_number  = VALUE #( ident_numbers = VALUE #(
          ( task     = 'I'
            data_key = VALUE #( identificationcategory = 'ZCUST'
                                identificationnumber   = |{ is_noc_partner_data-partn_numb ALPHA = OUT }| )
            data     = VALUE #( idinstitute            = |{ is_order_header-sales_org }/{ is_order_header-distr_chan }/{ is_order_header-division }| ) )
          ( task     = 'I'
            data_key = VALUE #( identificationcategory = 'ZTECH'
                                identificationnumber   = get_related_beu_sales_org( iv_noc_vkorg ) )
            data     = VALUE #( ) )
           ) )
        partner-central_data-address = VALUE #(
          current_state = 'C'
          time_dependent = abap_false
          addresses = VALUE #( (
             task               = 'I'
             data_key-operation = 'I'
             currently_valid    = abap_true
             data               = VALUE #(
                                    postal = VALUE #(
                                               data = VALUE #( BASE CORRESPONDING #( ls_address ) standardaddress = 'X'  )
                                                    )
                                    remark = VALUE #(
                                      current_state = 'C'
                                      remarks = VALUE #(
                                                ( task = 'I'
                                                  data = VALUE #(
                                                           langu     = 'E'
                                                           langu_iso = 'EN'
                                                           adr_notes = 'automatically created'(aen) ) )
                                                ( task = 'I'
                                                  data = VALUE #(
                                                           langu     = 'D'
                                                           langu_iso = 'DE'
                                                           adr_notes = 'Automatisch angelegt'(ade) ) )
                                                   )
                                                   )
                                    addr_usage-current_state = 'C'
                                    addr_usage-addr_usages   = VALUE #(
                                       ( task                 = 'I'
                                         currently_valid      = 'X'
                                         data_key-addresstype = 'XXDEFAULT' "'FIRMA' " '0003' "business address
                                         data-standard        = abap_true )
                                       ( task                 = 'I'
                                         currently_valid      = 'X'
                                         data_key-addresstype = 'FIRMA' " '0003' "business address
                                         data-standard        = abap_true )
                                                )
                                     )
                                    )
                                   )
                                  )
      customer-header-object_instance-kunnr = space
      customer-header-object_task           = 'I'
      customer-central_data-central-data    = VALUE #( ktokd = 'KM91' ) "ZKLI
      customer-central_data-central-datax   = VALUE #( ktokd = 'X' )    "ZKLI
      ensure_create-create_customer         = abap_true
      customer-sales_data-sales             = VALUE #(
                                      ( task = 'I'
                                        data_key-vkorg      = is_order_header-sales_org
                                        data_key-vtweg      = is_order_header-distr_chan
                                        data_key-spart      = is_order_header-division
                                        data-lprio          = '2'  "Delivery priority
                                        data-vsbed          = '01' "shipping conditions
                                        data-antlf          = '9'  "maximum number of permitted part deliveries per item
*                                        data-inco1          = ls_ecc_customer-inco1
*                                        data-inco2          = ls_ecc_customer-inco2
*                                        data-z_batch_determ = ls_ecc_customer-kvgr4
                                        functions-functions = VALUE #( ( data_key-parvw = 'WE' ) ) )
                                      ) ) .

    rv_bu_partner = book_business_partner( lx_data ).

  ENDMETHOD.


  METHOD find_customer_by_id.

    "use local variable to manipulate given id
    DATA(lv_bu_id_number) = iv_ext_id.

    "external ID is stored without any leading zeros or spaces
    SHIFT lv_bu_id_number LEFT DELETING LEADING '0'.
    SHIFT lv_bu_id_number LEFT DELETING LEADING space.

    "find business partner for type ZCUST
    SELECT SINGLE partner
      FROM but0id
      INTO @rv_bu_partner
     WHERE type     = 'ZCUST'
       AND idnumber = @lv_bu_id_number.
    IF sy-subrc > 0.
      CLEAR rv_bu_partner.
    ENDIF.

  ENDMETHOD.


  METHOD find_customer_in_s_area_by_id.

    "use local variable to manipulate given id
    DATA(lv_bu_id_number) = iv_ext_id.

    "external ID is stored without any leading zeros or spaces
    SHIFT lv_bu_id_number LEFT DELETING LEADING '0'.
    SHIFT lv_bu_id_number LEFT DELETING LEADING space.

    "find business partner for type ZCUST
    SELECT SINGLE partner
      FROM but0id
      INTO @rv_bu_partner
     WHERE type     = 'ZCUST'
       AND idnumber = @lv_bu_id_number.
    IF sy-subrc > 0.
      "Partner does not exist
      RAISE EXCEPTION TYPE zcx_bc_bp
        EXPORTING
          mv_partner = CONV #( lv_bu_id_number ).
    ELSEIF is_deleted( rv_bu_partner ).
      "Partner has been deleted
      RAISE EXCEPTION TYPE zcx_bc_bp
        EXPORTING
          mv_partner = CONV #( rv_bu_partner ).
    ELSE.
      "check sales area
      IF check_sales_area(
        EXPORTING
          iv_bu_partner = rv_bu_partner
          iv_vkorg      = iv_vkorg
          iv_vtweg      = iv_vtweg
          iv_spart      = iv_spart ) = abap_false.
        "partner does not exist in sales area - partner is not valid
        RAISE EXCEPTION TYPE zcx_bc_bp_sales_area
          EXPORTING
            textid           = zcx_bc_bp_sales_area=>sales_area_wrong
            iv_partner       = rv_bu_partner
            iv_sales_area    = iv_vkorg
            iv_distr_channel = iv_vtweg
            iv_division      = iv_spart.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD find_vendor_by_id.

    "use local variable to manipulate given id
    DATA(lv_bu_id_number) = iv_ext_id.

    "external ID is stored without any leading zeros or spaces
    SHIFT lv_bu_id_number LEFT DELETING LEADING '0'.
    SHIFT lv_bu_id_number LEFT DELETING LEADING space.

    "find business partner for type ZCUST
    SELECT SINGLE partner
      FROM but0id
      INTO @rv_bu_partner
     WHERE type     = 'ZVEND'
       AND idnumber = @lv_bu_id_number.
    IF sy-subrc > 0.
      CLEAR rv_bu_partner.
    ENDIF.

  ENDMETHOD.


  METHOD find_vendor_in_comp_code_by_id.

    "use local variable to manipulate given id
    DATA(lv_bu_id_number) = iv_ext_id.
    DATA(lv_comp_code)    = iv_bukrs.

    IF lv_comp_code IS INITIAL AND iv_sales_org IS NOT INITIAL.
      "derive company code from sales order definition
      SELECT SINGLE bukrs FROM tvko INTO lv_comp_code WHERE vkorg = iv_sales_org.
    ENDIF.

    "external ID is stored without any leading zeros or spaces
    SHIFT lv_bu_id_number LEFT DELETING LEADING '0'.
    SHIFT lv_bu_id_number LEFT DELETING LEADING space.

    "find business partner for type ZCUST
    SELECT SINGLE partner
      FROM but0id
      INTO @rv_bu_partner
     WHERE type     = 'ZVEND'
       AND idnumber = @lv_bu_id_number.
    IF sy-subrc > 0.
      CLEAR rv_bu_partner.
    ELSE.
      "check if vendor is assigned to desired company code
      IF check_company_code(
          iv_bu_partner = rv_bu_partner
          iv_bukrs      = lv_comp_code ) = abap_false.
        "vendor not assigned to company code - not valid.
        CLEAR rv_bu_partner.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_customer_id.

    "find business partner for type ZCUST
    SELECT SINGLE idnumber
      FROM but0id
      INTO @rv_ext_id
     WHERE partner  = @iv_bu_partner
       AND type     = 'ZCUST'.
    IF sy-subrc > 0.
      CLEAR rv_ext_id.
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


  METHOD get_related_beu_sales_org.

*    rv_beu_vkorg = zcl_sd_sales_org=>get_related_beu_sales_org( iv_noc_vkorg ).
    IF rv_beu_vkorg IS INITIAL.
      "technical identification missing
      MESSAGE e090(zcore) WITH iv_noc_vkorg.
      RAISE EXCEPTION TYPE zcx_bc_bp
        EXPORTING
          textid = VALUE scx_t100key(
            msgid = sy-msgid
            msgno = sy-msgno
            attr1 = sy-msgv1
            attr2 = sy-msgv2
            attr3 = sy-msgv3
            attr4 = sy-msgv4 ).
    ENDIF.

  ENDMETHOD.


  METHOD get_vendor_id.

    "find business partner for type ZCUST
    SELECT SINGLE idnumber
      FROM but0id
      INTO @rv_ext_id
     WHERE partner  = @iv_bu_partner
       AND type     = 'ZVEND'.
    IF sy-subrc > 0.
      CLEAR rv_ext_id.
    ENDIF.
  ENDMETHOD.


  METHOD is_deleted.

    SELECT SINGLE xdele
      FROM but000 INTO rv_deleted
     WHERE partner = iv_bu_partner.

  ENDMETHOD.


  METHOD lock.

    CALL FUNCTION 'ENQUEUE_EBU_PARTNR'
      EXPORTING
        mode_but000    = 'E'              " Lock mode for table BUT000
        client         = sy-mandt         " Enqueue argument 01
        partner        = iv_bu_partner                  " Enqueue argument 02
        _wait          = 'X'
      EXCEPTIONS
        foreign_lock   = 1                " Object already locked
        system_failure = 2                " Internal error from enqueue server
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO DATA(lv_message).
      RAISE EXCEPTION TYPE zcx_bc_bp_locked
        EXPORTING
          object  = CONV #( iv_bu_partner )
          message = lv_message.
    ENDIF.

  ENDMETHOD.


  METHOD unlock.

    CALL FUNCTION 'DEQUEUE_EBU_PARTNR'
      EXPORTING
        mode_but000 = 'E'              " Lock mode for table BUT000
        client      = sy-mandt         " Enqueue argument 01
        partner     = iv_bu_partner.

  ENDMETHOD.
ENDCLASS.
