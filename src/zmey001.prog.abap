REPORT z_doppelte_alv_liste_report.

CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS run.
    METHODS pbo_0100.
    METHODS pai_0100 IMPORTING iv_ucomm TYPE sy-ucomm.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_master,
             master_id TYPE c LENGTH 5,
             name      TYPE c LENGTH 30,
             category  TYPE c LENGTH 15,
             status    TYPE c LENGTH 10,
           END OF ty_master.

    TYPES: BEGIN OF ty_detail,
             master_id TYPE c LENGTH 5,
             item_no   TYPE n LENGTH 3,
             item_text TYPE c LENGTH 40,
             amount    TYPE p LENGTH 10 DECIMALS 2,
             currency  TYPE c LENGTH 3,
           END OF ty_detail.

    DATA: mt_master        TYPE STANDARD TABLE OF ty_master,
          mt_detail_all    TYPE STANDARD TABLE OF ty_detail,
          mt_detail_view   TYPE STANDARD TABLE OF ty_detail,
          mo_main          TYPE REF TO cl_gui_custom_container,
          mo_top_container TYPE REF TO cl_gui_container,
          mo_bot_container TYPE REF TO cl_gui_container,
          mo_splitter      TYPE REF TO cl_gui_splitter_container,
          mo_salv_master   TYPE REF TO cl_salv_table,
          mo_salv_detail   TYPE REF TO cl_salv_table,
          mv_built         TYPE abap_bool.

    METHODS fill_test_data.
    METHODS build_ui_once.
    METHODS build_master_alv.
    METHODS build_detail_alv.
    METHODS fill_initial_detail.
    METHODS refresh_detail_for_master
      IMPORTING iv_master_id TYPE ty_master-master_id.
    METHODS set_alv_defaults
      IMPORTING io_salv  TYPE REF TO cl_salv_table
                iv_title TYPE string.
    METHODS on_master_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD run.
    fill_test_data( ).
    CALL SCREEN 0100.
  ENDMETHOD.

  METHOD fill_test_data.
    mt_master = VALUE #(
      ( master_id = 'M0001' name = 'Vertrag A' category = 'Standard' status = 'AKTIV' )
      ( master_id = 'M0002' name = 'Vertrag B' category = 'Premium'  status = 'AKTIV' )
      ( master_id = 'M0003' name = 'Vertrag C' category = 'Basic'    status = 'INAKTIV' )
      ( master_id = 'M0004' name = 'Vertrag D' category = 'Premium'  status = 'AKTIV' ) ).

    mt_detail_all = VALUE #(
      ( master_id = 'M0001' item_no = '001' item_text = 'Leistung 1' amount = '120.50' currency = 'EUR' )
      ( master_id = 'M0001' item_no = '002' item_text = 'Leistung 2' amount = '89.90'  currency = 'EUR' )
      ( master_id = 'M0002' item_no = '001' item_text = 'Service A'  amount = '199.00' currency = 'EUR' )
      ( master_id = 'M0002' item_no = '002' item_text = 'Service B'  amount = '49.99'  currency = 'EUR' )
      ( master_id = 'M0003' item_no = '001' item_text = 'Paket X'    amount = '15.00'  currency = 'EUR' )
      ( master_id = 'M0004' item_no = '001' item_text = 'Option Q'   amount = '350.00' currency = 'EUR' )
      ( master_id = 'M0004' item_no = '002' item_text = 'Option R'   amount = '75.50'  currency = 'EUR' ) ).
  ENDMETHOD.

  METHOD pbo_0100.
    SET PF-STATUS 'ZHR_MEY001'.
    IF mv_built = abap_false.
      build_ui_once( ).
      mv_built = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD pai_0100.
    CASE iv_ucomm.
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
        LEAVE TO SCREEN 0.
    ENDCASE.
  ENDMETHOD.

  METHOD build_ui_once.
    CREATE OBJECT mo_main
      EXPORTING
        container_name = 'CC_MAIN'.

    CREATE OBJECT mo_splitter
      EXPORTING
        parent  = mo_main
        rows    = 2
        columns = 1.

    mo_splitter->set_row_height( id = 1 height = 45 ).
    mo_splitter->set_row_height( id = 2 height = 55 ).

    mo_top_container = mo_splitter->get_container( row = 1 column = 1 ).
    mo_bot_container = mo_splitter->get_container( row = 2 column = 1 ).

    build_master_alv( ).
    fill_initial_detail( ).
    build_detail_alv( ).
  ENDMETHOD.

  METHOD set_alv_defaults.
    DATA: lo_columns TYPE REF TO cl_salv_columns_table,
          lo_display TYPE REF TO cl_salv_display_settings,
          lo_funcs   TYPE REF TO cl_salv_functions_list,
          lo_layout  TYPE REF TO cl_salv_layout,
          ls_key     TYPE salv_s_layout_key.

    lo_columns = io_salv->get_columns( ).
    lo_columns->set_optimize( abap_true ).

    lo_display = io_salv->get_display_settings( ).
    lo_display->set_striped_pattern( abap_true ).
    lo_display->set_list_header( 'Sven' ).

    lo_funcs = io_salv->get_functions( ).
    lo_funcs->set_all( abap_true ).

    lo_layout = io_salv->get_layout( ).
    ls_key-report = sy-repid.
    lo_layout->set_key( ls_key ).
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  ENDMETHOD.

  METHOD build_master_alv.
    DATA lo_events TYPE REF TO cl_salv_events_table.

    cl_salv_table=>factory(
      EXPORTING
        r_container  = mo_top_container
      IMPORTING
        r_salv_table = mo_salv_master
      CHANGING
        t_table      = mt_master ).

    set_alv_defaults(
      io_salv  = mo_salv_master
      iv_title = 'Master-Liste (oben) - Doppelklick lädt Details unten' ).

    lo_events = mo_salv_master->get_event( ).
    SET HANDLER on_master_double_click FOR lo_events.

    mo_salv_master->display( ).
  ENDMETHOD.

  METHOD fill_initial_detail.
    DATA ls_master TYPE ty_master.

    READ TABLE mt_master INTO ls_master INDEX 1.
    IF sy-subrc = 0.
      refresh_detail_for_master( ls_master-master_id ).
    ENDIF.
  ENDMETHOD.

  METHOD build_detail_alv.
    cl_salv_table=>factory(
      EXPORTING
        r_container  = mo_bot_container
      IMPORTING
        r_salv_table = mo_salv_detail
      CHANGING
        t_table      = mt_detail_view ).

    set_alv_defaults(
      io_salv  = mo_salv_detail
      iv_title = 'Detail-Liste (unten)' ).

    mo_salv_detail->display( ).
  ENDMETHOD.

  METHOD refresh_detail_for_master.
    CLEAR mt_detail_view.
    LOOP AT mt_detail_all ASSIGNING FIELD-SYMBOL(<ls_detail>) WHERE master_id = iv_master_id.
      APPEND <ls_detail> TO mt_detail_view.
    ENDLOOP.

    IF mo_salv_detail IS BOUND.
      mo_salv_detail->get_display_settings( )->set_list_header(
        |Detail-Liste (unten) zu Master-ID { iv_master_id }| ).
      mo_salv_detail->refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD on_master_double_click.
    DATA ls_master TYPE ty_master.

    READ TABLE mt_master INTO ls_master INDEX row.
    IF sy-subrc = 0.
      refresh_detail_for_master( ls_master-master_id ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

DATA go_app TYPE REF TO lcl_app.

START-OF-SELECTION.
  CREATE OBJECT go_app.
  go_app->run( ).

MODULE status_0100 OUTPUT.
  go_app->pbo_0100( ).
ENDMODULE.

MODULE user_command_0100 INPUT.
  go_app->pai_0100( sy-ucomm ).
ENDMODULE.
