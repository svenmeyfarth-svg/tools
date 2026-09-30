CLASS zcl_hr_bonus_factor DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      ty_t_awart TYPE SORTED TABLE OF p2001-awart
        WITH UNIQUE KEY table_line,
      BEGIN OF ty_day,
        datum         TYPE d,
        tprog         TYPE ptpsp-tprog,
        sollstunden   TYPE ptpsp-stdaz,
        solltag       TYPE i,
        unbezahlt_tag TYPE i,
      END OF ty_day,
      ty_t_day TYPE SORTED TABLE OF ty_day WITH UNIQUE KEY datum,
      BEGIN OF ty_month,
        monat           TYPE n LENGTH 6,
        solltage        TYPE i,
        unbezahlte_tage TYPE i,
        anrechenbar     TYPE i,
        faktor          TYPE decfloat34,
        faktor_gueltig  TYPE abap_bool,
      END OF ty_month,
      ty_t_month TYPE SORTED TABLE OF ty_month WITH UNIQUE KEY monat.

    CLASS-METHODS get_factors
      IMPORTING
        iv_pernr        TYPE pernr_d
        iv_begda        TYPE begda
        iv_endda        TYPE endda
        it_unpaid_awart TYPE ty_t_awart
      EXPORTING
        et_months       TYPE ty_t_month
        et_days         TYPE ty_t_day
      EXCEPTIONS
        invalid_input
        infotype_error
        schedule_error.

  PRIVATE SECTION.
    CLASS-METHODS read_infotype
      IMPORTING
        iv_pernr TYPE pernr_d
        iv_infty TYPE infty
        iv_begda TYPE begda
        iv_endda TYPE endda
      CHANGING
        ct_data  TYPE STANDARD TABLE
      EXCEPTIONS
        read_error.
ENDCLASS.

CLASS zcl_hr_bonus_factor IMPLEMENTATION.
  METHOD read_infotype.
    DATA lv_subrc TYPE sy-subrc.
    CLEAR ct_data.

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = iv_pernr
        infty           = iv_infty
        begda           = iv_begda
        endda           = iv_endda
      IMPORTING
        subrc           = lv_subrc
      TABLES
        infty_tab       = ct_data
      EXCEPTIONS
        infty_not_found = 1
        OTHERS          = 2.

    " SUBRC 4: kein Datensatz im Zeitraum.
    " Fehlende Pflichtdaten prueft die Planerzeugung.
    IF sy-subrc <> 0 OR ( lv_subrc <> 0 AND lv_subrc <> 4 ).
      RAISE read_error.
    ENDIF.

    LOOP AT ct_data ASSIGNING FIELD-SYMBOL(<record>).
      ASSIGN COMPONENT 'SPRPS' OF STRUCTURE <record>
        TO FIELD-SYMBOL(<locked>).
      IF sy-subrc = 0.
        IF <locked> = 'X'.
          DELETE ct_data.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_factors.
    DATA:
      lt_0000    TYPE STANDARD TABLE OF p0000,
      lt_0001    TYPE STANDARD TABLE OF p0001,
      lt_0002    TYPE STANDARD TABLE OF p0002,
      lt_0007    TYPE STANDARD TABLE OF p0007,
      lt_2001    TYPE STANDARD TABLE OF p2001,
      lt_2003    TYPE STANDARD TABLE OF p2003,
      lt_no_abs  TYPE STANDARD TABLE OF p2001,
      lt_no_att  TYPE STANDARD TABLE OF p2002,
      lt_perws   TYPE STANDARD TABLE OF ptpsp,
      lt_days    TYPE ty_t_day,
      lt_months  TYPE ty_t_month,
      lv_date    TYPE d,
      lv_month   TYPE n LENGTH 6,
      lv_warning TYPE c LENGTH 1.

    CLEAR: et_months, et_days.
    IF iv_pernr IS INITIAL
       OR iv_begda IS INITIAL OR iv_endda IS INITIAL
       OR iv_begda > iv_endda
       OR iv_begda(4) <> iv_endda(4)
       OR it_unpaid_awart IS INITIAL.
      RAISE invalid_input.
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = iv_begda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE invalid_input.
    ENDIF.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = iv_endda
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      RAISE invalid_input.
    ENDIF.

    DEFINE read_it.
      read_infotype(
        EXPORTING
          iv_pernr = iv_pernr
          iv_infty = &1
          iv_begda = iv_begda
          iv_endda = iv_endda
        CHANGING
          ct_data = &2
        EXCEPTIONS
          read_error = 1 ).
      IF sy-subrc <> 0.
        RAISE infotype_error.
      ENDIF.
    END-OF-DEFINITION.

    read_it '0000' lt_0000.
    read_it '0001' lt_0001.
    read_it '0002' lt_0002.
    read_it '0007' lt_0007.
    read_it '2001' lt_2001.
    read_it '2003' lt_2003.

    " Sollbasis ohne Abwesenheiten; Vertretungen bleiben erhalten.
    CALL FUNCTION 'HR_PERSONAL_WORK_SCHEDULE'
      EXPORTING
        pernr           = iv_pernr
        begda           = iv_begda
        endda           = iv_endda
        refresh         = 'X'
        working_hours   = 'X'
      IMPORTING
        warning_occured = lv_warning
      TABLES
        i0000           = lt_0000
        i0001           = lt_0001
        i0002           = lt_0002
        i0007           = lt_0007
        i2001           = lt_no_abs
        i2002           = lt_no_att
        i2003           = lt_2003
        perws           = lt_perws
      EXCEPTIONS
        error_occured   = 1
        abort_occured   = 2
        OTHERS          = 3.
    IF sy-subrc <> 0 OR lv_warning IS NOT INITIAL.
      RAISE schedule_error.
    ENDIF.

    " Untertaegige und nicht zugelassene Abwesenheiten ignorieren.
    LOOP AT lt_2001 ASSIGNING FIELD-SYMBOL(<absence>).
      IF <absence>-alldf <> abap_true.
        DELETE lt_2001.
        CONTINUE.
      ENDIF.
      READ TABLE it_unpaid_awart
        WITH TABLE KEY table_line = <absence>-awart
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE lt_2001.
      ENDIF.
    ENDLOOP.

    lv_date = iv_begda.
    DO.
      READ TABLE lt_perws INTO DATA(ls_plan)
        WITH KEY datum = lv_date.
      IF sy-subrc <> 0.
        RAISE schedule_error.
      ENDIF.

      DATA(ls_day) = VALUE ty_day(
        datum       = lv_date
        tprog       = ls_plan-tprog
        sollstunden = ls_plan-stdaz ).
      IF ls_plan-stdaz > 0.
        ls_day-solltag = 1.
        LOOP AT lt_2001 TRANSPORTING NO FIELDS
          WHERE begda <= lv_date AND endda >= lv_date.
          ls_day-unbezahlt_tag = 1.
          EXIT. " Jeden Arbeitstag hoechstens einmal kuerzen
        ENDLOOP.
      ENDIF.
      INSERT ls_day INTO TABLE lt_days.

      lv_month = lv_date(6).
      READ TABLE lt_months ASSIGNING FIELD-SYMBOL(<month>)
        WITH TABLE KEY monat = lv_month.
      IF sy-subrc <> 0.
        INSERT VALUE #( monat = lv_month )
          INTO TABLE lt_months ASSIGNING <month>.
      ENDIF.
      <month>-solltage = <month>-solltage + ls_day-solltag.
      <month>-unbezahlte_tage =
        <month>-unbezahlte_tage + ls_day-unbezahlt_tag.

      IF lv_date = iv_endda.
        EXIT.
      ENDIF.
      lv_date = lv_date + 1.
    ENDDO.

    LOOP AT lt_months ASSIGNING <month>.
      <month>-anrechenbar =
        <month>-solltage - <month>-unbezahlte_tage.
      IF <month>-solltage > 0.
        <month>-faktor = CONV decfloat34( <month>-anrechenbar )
                        / CONV decfloat34( <month>-solltage ).
        <month>-faktor_gueltig = abap_true.
      ELSE.
        " Ohne Solltage ist der Faktor nicht definiert.
        CLEAR: <month>-faktor, <month>-faktor_gueltig.
      ENDIF.
    ENDLOOP.
    et_days = lt_days.
    et_months = lt_months.
  ENDMETHOD.
ENDCLASS.
