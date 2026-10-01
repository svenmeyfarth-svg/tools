CLASS ltc_bonus_factor DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS entry_exit_reentry FOR TESTING.
    METHODS employment_events FOR TESTING.
    METHODS inactive_is_employed FOR TESTING.
    METHODS no_history_is_error FOR TESTING.
    METHODS monthly_deductions FOR TESTING.
    METHODS zero_scheduled_days FOR TESTING.
    METHODS daily_percent_changes FOR TESTING.
    METHODS weighted_month FOR TESTING.
    METHODS missing_percent FOR TESTING.
ENDCLASS.

CLASS ltc_bonus_factor IMPLEMENTATION.
  METHOD entry_exit_reentry.
    DATA(lt_actions) = VALUE zcl_hr_bonus_factor=>ty_t_actions(
      ( begda = '20260115' endda = '20260310' stat2 = '3' )
      ( begda = '20260311' endda = '20260414' stat2 = '0' )
      ( begda = '20260415' endda = '99991231' stat2 = '3' ) ).
    TYPES: BEGIN OF ty_case,
             datum TYPE d,
             reference TYPE d,
             employed TYPE abap_bool,
           END OF ty_case.
    DATA lt_cases TYPE STANDARD TABLE OF ty_case.
    lt_cases = VALUE #(
      ( datum = '20260114' reference = '20260115' employed = abap_false )
      ( datum = '20260115' reference = '20260115' employed = abap_true )
      ( datum = '20260310' reference = '20260310' employed = abap_true )
      ( datum = '20260311' reference = '20260310' employed = abap_false )
      ( datum = '20260414' reference = '20260310' employed = abap_false )
      ( datum = '20260415' reference = '20260415' employed = abap_true ) ).
    LOOP AT lt_cases INTO DATA(ls_case).
      zcl_hr_bonus_factor=>get_employment_reference(
        EXPORTING it_actions = lt_actions iv_date = ls_case-datum
        IMPORTING ev_reference = DATA(lv_reference)
                  ev_employed = DATA(lv_employed)
        EXCEPTIONS missing_employment = 1 ).
      cl_abap_unit_assert=>assert_subrc( ).
      cl_abap_unit_assert=>assert_equals(
        act = lv_reference exp = ls_case-reference ).
      cl_abap_unit_assert=>assert_equals(
        act = lv_employed exp = ls_case-employed ).
    ENDLOOP.
  ENDMETHOD.

  METHOD inactive_is_employed.
    DATA(lt_actions) = VALUE zcl_hr_bonus_factor=>ty_t_actions(
      ( begda = '20260101' endda = '20261231' stat2 = '1' ) ).
    zcl_hr_bonus_factor=>get_employment_reference(
      EXPORTING it_actions = lt_actions iv_date = '20260301'
      IMPORTING ev_employed = DATA(lv_employed)
      EXCEPTIONS missing_employment = 1 ).
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_true( lv_employed ).
  ENDMETHOD.

  METHOD no_history_is_error.
    DATA lt_actions TYPE zcl_hr_bonus_factor=>ty_t_actions.
    zcl_hr_bonus_factor=>get_employment_reference(
      EXPORTING it_actions = lt_actions iv_date = '20260101'
      EXCEPTIONS missing_employment = 1 ).
    cl_abap_unit_assert=>assert_subrc( exp = 1 ).
  ENDMETHOD.

  METHOD monthly_deductions.
    " 4 Solltage: 1 ausserhalb, 1 unbezahlte Abwesenheit, 2 anrechenbar.
    " Arbeitsfreier Tag ausserhalb zaehlt nicht als Kuerzungstag.
    DATA(lt_days) = VALUE zcl_hr_bonus_factor=>ty_t_day(
      ( datum = '20260101' solltag = 1 unbezahlt_tag = 1 ausserhalb_tag = 1 )
      ( datum = '20260102' solltag = 1 unbezahlt_tag = 1
        abwesenheit_tag = 1 beschaeftigt = abap_true )
      ( datum = '20260103' solltag = 0 )
      ( datum = '20260105' solltag = 1 beschaeftigt = abap_true
        bsgrd = 100 bsgrd_gueltig = abap_true )
      ( datum = '20260106' solltag = 1 beschaeftigt = abap_true
        bsgrd = 100 bsgrd_gueltig = abap_true )
      ( datum = '20260202' solltag = 1 unbezahlt_tag = 1 ausserhalb_tag = 1 ) ).
    DATA(lt_months) = zcl_hr_bonus_factor=>summarize_days( lt_days ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_months ) exp = 2 ).
    DATA(ls_jan) = lt_months[ monat = '202601' ].
    cl_abap_unit_assert=>assert_equals( act = ls_jan-solltage exp = 4 ).
    cl_abap_unit_assert=>assert_equals( act = ls_jan-unbezahlte_tage exp = 2 ).
    cl_abap_unit_assert=>assert_equals( act = ls_jan-ausserhalb_tage exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_jan-abwesenheit_tage exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_jan-anrechenbar exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_jan-faktor exp = CONV decfloat34( '0.5' ) ).
    cl_abap_unit_assert=>assert_true( ls_jan-faktor_gueltig ).
    DATA(ls_feb) = lt_months[ monat = '202602' ].
    cl_abap_unit_assert=>assert_equals( act = ls_feb-faktor exp = 0 ).
    cl_abap_unit_assert=>assert_true( ls_feb-faktor_gueltig ).
  ENDMETHOD.

  METHOD zero_scheduled_days.
    DATA(lt_days) = VALUE zcl_hr_bonus_factor=>ty_t_day(
      ( datum = '20260101' )
      ( datum = '20260201' beschaeftigt = abap_true ) ).
    DATA(lt_months) = zcl_hr_bonus_factor=>summarize_days( lt_days ).
    cl_abap_unit_assert=>assert_true( lt_months[ monat = '202601' ]-faktor_gueltig ).
    cl_abap_unit_assert=>assert_false( lt_months[ monat = '202602' ]-faktor_gueltig ).
  ENDMETHOD.
  METHOD daily_percent_changes.
    DATA(lt_pay) = VALUE zcl_hr_bonus_factor=>ty_t_pay(
      ( begda = '20260101' endda = '20260115' bsgrd = 50 )
      ( begda = '20260116' endda = '20260131' bsgrd = 80 ) ).
    zcl_hr_bonus_factor=>get_employment_percent(
      EXPORTING it_pay = lt_pay iv_date = '20260115'
      IMPORTING ev_percent = DATA(lv_percent) ev_valid = DATA(lv_valid) ).
    cl_abap_unit_assert=>assert_equals( act = lv_percent exp = 50 ).
    cl_abap_unit_assert=>assert_true( lv_valid ).
    zcl_hr_bonus_factor=>get_employment_percent(
      EXPORTING it_pay = lt_pay iv_date = '20260116'
      IMPORTING ev_percent = lv_percent ev_valid = lv_valid ).
    cl_abap_unit_assert=>assert_equals( act = lv_percent exp = 80 ).
    cl_abap_unit_assert=>assert_true( lv_valid ).
    zcl_hr_bonus_factor=>get_employment_percent(
      EXPORTING it_pay = lt_pay iv_date = '20260201'
      IMPORTING ev_percent = lv_percent ev_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( lv_valid ).
    APPEND VALUE #( begda = '20260101' endda = '20260131' bsgrd = 100 ) TO lt_pay.
    zcl_hr_bonus_factor=>get_employment_percent(
      EXPORTING it_pay = lt_pay iv_date = '20260115'
      IMPORTING ev_valid = lv_valid ).
    cl_abap_unit_assert=>assert_false( lv_valid ).
  ENDMETHOD.

  METHOD weighted_month.
    DATA(lt_days) = VALUE zcl_hr_bonus_factor=>ty_t_day(
      ( datum = '20260101' solltag = 1 beschaeftigt = abap_true
        bsgrd = 50 bsgrd_gueltig = abap_true )
      ( datum = '20260102' solltag = 1 beschaeftigt = abap_true
        bsgrd = 100 bsgrd_gueltig = abap_true )
      ( datum = '20260105' solltag = 1 beschaeftigt = abap_true
        bsgrd = 80 bsgrd_gueltig = abap_true unbezahlt_tag = 1 abwesenheit_tag = 1 )
      ( datum = '20260106' solltag = 1 unbezahlt_tag = 1 ausserhalb_tag = 1 ) ).
    DATA(lt_months) = zcl_hr_bonus_factor=>summarize_days( lt_days ).
    DATA(ls_month) = lt_months[ monat = '202601' ].
    cl_abap_unit_assert=>assert_equals(
      act = ls_month-gewichtet exp = CONV decfloat34( '1.5' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_month-faktor exp = CONV decfloat34( '0.375' ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_month-bsgrd_durchschnitt exp = 75 ).
    cl_abap_unit_assert=>assert_true( ls_month-faktor_gueltig ).
  ENDMETHOD.

  METHOD missing_percent.
    DATA(lt_days) = VALUE zcl_hr_bonus_factor=>ty_t_day(
      ( datum = '20260101' solltag = 1 beschaeftigt = abap_true )
      ( datum = '20260201' solltag = 1 beschaeftigt = abap_true
        bsgrd = 0 bsgrd_gueltig = abap_true ) ).
    DATA(lt_months) = zcl_hr_bonus_factor=>summarize_days( lt_days ).
    cl_abap_unit_assert=>assert_false( lt_months[ monat = '202601' ]-faktor_gueltig ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_months[ monat = '202601' ]-bsgrd_fehlende_tage exp = 1 ).
    cl_abap_unit_assert=>assert_true( lt_months[ monat = '202602' ]-faktor_gueltig ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_months[ monat = '202602' ]-faktor exp = 0 ).
  ENDMETHOD.

  METHOD employment_events.
    DATA(lt_actions) = VALUE zcl_hr_bonus_factor=>ty_t_actions(
      ( begda = '20260101' endda = '20260114' stat2 = '3' )
      ( begda = '20260115' endda = '20260131' stat2 = '1' )
      ( begda = '20260201' endda = '20260310' stat2 = '3' )
      ( begda = '20260311' endda = '20260414' stat2 = '0' )
      ( begda = '20260415' endda = '20261231' stat2 = '3' )
      ( begda = '20270101' endda = '99991231' stat2 = '0' ) ).
    DATA(lt_events) = zcl_hr_bonus_factor=>get_employment_events(
      it_actions = lt_actions iv_begda = '20260101' iv_endda = '20261231' ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_events ) exp = 4 ).
    cl_abap_unit_assert=>assert_true(
      xsdbool( line_exists( lt_events[ datum = '20260101' ereignis = 'E' ] ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_events[ datum = '20260310' ereignis = 'A' ]-statusdatum
      exp = CONV d( '20260311' ) ).
    cl_abap_unit_assert=>assert_true(
      xsdbool( line_exists( lt_events[ datum = '20260415' ereignis = 'E' ] ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_events[ datum = '20261231' ereignis = 'A' ]-statusdatum
      exp = CONV d( '20270101' ) ).
    lt_events = zcl_hr_bonus_factor=>get_employment_events(
      it_actions = lt_actions iv_begda = '20260102' iv_endda = '20260309' ).
    cl_abap_unit_assert=>assert_initial( lt_events ).
    " Fehlender Folgesatz darf keinen erfundenen Austritt erzeugen.
    DELETE lt_actions WHERE stat2 = '0'.
    lt_events = zcl_hr_bonus_factor=>get_employment_events(
      it_actions = lt_actions iv_begda = '20261231' iv_endda = '20261231' ).
    cl_abap_unit_assert=>assert_initial( lt_events ).
  ENDMETHOD.
ENDCLASS.
