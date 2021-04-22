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

    DATA ls_partner_data TYPE bapiparnr.
    DATA ls_order_header TYPE bapisdhd1.
    DATA ls_address TYPE bapiaddr1.

    "prepare test data
    ls_order_header-sales_org      = 'RS10'.
    ls_order_header-distr_chan     = 'R1'.
    ls_order_header-division       = 'R1'.

    ls_address-build_long          = 'B12'. "building
    ls_address-building            = 'B12'. "building
    ls_address-floor               = '12'.  "floor
    ls_address-room_no             = '12B'. "room number
    ls_address-c_o_name            = 'Teddy Testuser'. "department
    ls_address-adr_notes           = 'Test remark'.    "remark

    ls_address-name                = 'Teddy Creations GmbH'.
    ls_address-city                = 'Kuschelhausen'.
    ls_address-postl_cod1          = '74447'.
    ls_address-street              = 'Knopfstraße'.
    ls_address-transpzone          = '0000000001'.
    ls_address-house_no            = '18 A'.
    ls_address-country             = 'DE'.
    ls_address-langu               = 'DE'.
    ls_address-sort1               = 'TEDDY'.


    TRY.
        "create customer for testing purposes
        DATA(lv_new_partner) = zcl_bp=>create_ship_to_party(
            is_address   = ls_address
            is_partner_data = ls_partner_data
            is_order_header = ls_order_header ).
      CATCH zcx_bc_bp INTO DATA(lo_error).
      CATCH zcx_bc_bp_locked INTO DATA(lo_locked).
    ENDTRY.

    cl_abap_unit_assert=>assert_not_bound( act = lo_error ).
    cl_abap_unit_assert=>assert_not_initial( act = lv_new_partner ).

*    SELECT SINGLE * FROM kna1 INTO @DATA(ls_new_partner_master)
*     WHERE kunnr = @lv_new_partner.
    SELECT SINGLE * FROM but000 INTO @DATA(ls_new_partner_master)
     WHERE partner = @lv_new_partner.

    cl_abap_unit_assert=>assert_initial( act = sy-subrc msg = |new partner { lv_new_partner } does not exist on KNA1| ).
    cl_abap_unit_assert=>assert_equals( act = ls_new_partner_master-bu_sort1  exp = ls_address-sort1 ).

*    SELECT SINGLE * FROM adrc INTO @DATA(ls_new_address)
*     WHERE addrnumber = @ls_new_partner_master-adrnr.
    DATA(lv_sales_area_exists) = zcl_bp=>check_sales_area(
      iv_bu_partner = lv_new_partner
      iv_vkorg      = 'RS10'
      iv_vtweg      = 'R1'
      iv_spart      = 'R1' ).
    cl_abap_unit_assert=>assert_not_initial( act = lv_sales_area_exists msg = |new partner { lv_new_partner } has no sales area view| ).

    "check address field that must not be taken into the master data
*    cl_abap_unit_assert=>assert_equals( act = ls_new_address-building  exp = space ).
*    cl_abap_unit_assert=>assert_equals( act = ls_new_address-name_co   exp = space ).
*    cl_abap_unit_assert=>assert_equals( act = ls_new_address-floor     exp = space ).
*    cl_abap_unit_assert=>assert_equals( act = ls_new_address-sort1     exp = ls_address-sort1 ).


*    SELECT SINGLE * FROM knvv INTO @DATA(ls_new_partner_sales)
*     WHERE kunnr = @lv_new_partner
*       AND vkorg = @ls_order_header-sales_org
*       AND vtweg = @ls_order_header-distr_chan
*       AND spart = @ls_order_header-division.
*    cl_abap_unit_assert=>assert_initial( act = sy-subrc ).


  ENDMETHOD.




ENDCLASS.
