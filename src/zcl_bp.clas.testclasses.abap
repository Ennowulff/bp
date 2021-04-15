*"* use this source file for your ABAP unit test classes

CLASS lcl_creation DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Creation
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_BP
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    METHODS: create_ship_to_party_salesarea FOR TESTING.
ENDCLASS.       "lcl_Creation


CLASS lcl_creation IMPLEMENTATION.



  METHOD create_ship_to_party_salesarea.

    DATA ls_noc_partner_data TYPE bapiparnr.
    DATA ls_order_header TYPE bapisdhd1.
    DATA ls_address TYPE bapiaddr1.

    "prepare test data
    ls_order_header-sales_org      = '1710'.
    ls_order_header-distr_chan     = '10'.
    ls_order_header-division       = '00'.

*    ls_noc_partner_data-partn_numb = '0000005371'."KAISER DRUCKTECHNIKEN

    ls_address-build_long          = 'B12'. "building
    ls_address-building            = 'B12'. "building
    ls_address-floor               = '12'. "floor
    ls_address-room_no             = '12B'. "room number
    ls_address-c_o_name            = 'Teddy Testuser'. "department
    ls_address-adr_notes           = 'Test remark'. "remark

    ls_address-name                = 'Kaiser Drucktechniken'.
    ls_address-city                = 'Bietigheim'.
    ls_address-postl_cod1          = '76467'.
    ls_address-street              = 'Sofienstraße'.
    ls_address-transpzone          = '0000000001'.
    ls_address-house_no            = '18'.
    ls_address-country             = 'DE'.
    ls_address-langu               = 'DE'.
    ls_address-sort1               = 'KAISER DRUCKTECHNIKE'.


    TRY.
        "create customer for testing purposes
        DATA(lv_new_partner) = zcl_bp=>create_ship_to_party(
            is_address   = ls_address
            is_noc_partner_data = ls_noc_partner_data
            iv_noc_vkorg = 'xxxx'
            iv_noc_vtweg = 'xx'
            iv_noc_spart = 'xx'
            is_order_header = ls_order_header ).
      CATCH zcx_bc_bp INTO DATA(lo_error).
      CATCH zcx_bc_bp_locked INTO DATA(lo_locked).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lo_error ).
    cl_abap_unit_assert=>assert_not_initial( act = lv_new_partner ).

    SELECT SINGLE * FROM kna1 INTO @DATA(ls_new_partner_master)
     WHERE kunnr = @lv_new_partner.

    cl_abap_unit_assert=>assert_initial( act = sy-subrc msg = |new partner { lv_new_partner } does not exist on KNA1| ).
    cl_abap_unit_assert=>assert_equals( act = ls_new_partner_master-sortl  exp = ls_address-sort1(10) ).

    SELECT SINGLE * FROM adrc INTO @DATA(ls_new_address)
     WHERE addrnumber = @ls_new_partner_master-adrnr.
    cl_abap_unit_assert=>assert_initial( act = sy-subrc msg = |new partner { lv_new_partner } has no sales area KNVV| ).

    "check address field that must not be taken into the master data
    cl_abap_unit_assert=>assert_equals( act = ls_new_address-building  exp = space ).
    cl_abap_unit_assert=>assert_equals( act = ls_new_address-name_co   exp = space ).
    cl_abap_unit_assert=>assert_equals( act = ls_new_address-floor     exp = space ).
    cl_abap_unit_assert=>assert_equals( act = ls_new_address-sort1     exp = ls_address-sort1 ).


    SELECT SINGLE * FROM knvv INTO @DATA(ls_new_partner_sales)
     WHERE kunnr = @lv_new_partner
       AND vkorg = @ls_order_header-sales_org
       AND vtweg = @ls_order_header-distr_chan
       AND spart = @ls_order_header-division.
    cl_abap_unit_assert=>assert_initial( act = sy-subrc ).

    "check data fetched by remote function for customer 5371 from EUD
*    cl_abap_unit_assert=>assert_equals( act = ls_new_partner_sales-z_batch_determ  exp = '02' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_new_partner_sales-inco1  exp = 'DDP' ).
*    cl_abap_unit_assert=>assert_equals( act = ls_new_partner_sales-inco2  exp = '.' ).


  ENDMETHOD.




ENDCLASS.
