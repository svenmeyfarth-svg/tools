CLASS zcl_hr_bonus_factor DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      ty_t_awart TYPE SORTED TABLE OF p2001-awart
        WITH UNIQUE KEY table_line,
      BEGIN OF ty_unpaid,
        awart TYPE p2001-awart,
        begda TYPE begda,
        endda TYPE endda,
      END OF ty_unpaid,
      ty_t_unpaid TYPE SORTED TABLE OF ty_unpaid
        WITH UNIQUE KEY awart begda endda,
      ty_t_actions TYPE STANDARD TABLE OF p0000 WITH DEFAULT KEY,
      ty_t_pay TYPE STANDARD TABLE OF p0008 WITH DEFAULT KEY,
      BEGIN OF ty_day,
        datum         TYPE d,
        tprog         TYPE ptpsp-tprog,
        sollstunden   TYPE ptpsp-stdaz,
        solltag       TYPE i,
        unbezahlt_tag TYPE i,
        beschaeftigt  TYPE abap_bool,
        ausserhalb_tag TYPE i,
        abwesenheit_tag TYPE i,
        bsgrd TYPE p0008-bsgrd,
        bsgrd_gueltig TYPE abap_bool,
        awart TYPE string,
        atext TYPE string,
      END OF ty_day,
      ty_t_day TYPE SORTED TABLE OF ty_day WITH UNIQUE KEY datum,
      BEGIN OF ty_month,
        monat           TYPE n LENGTH 6,
        solltage        TYPE i,
        unbezahlte_tage TYPE i,
        ausserhalb_tage TYPE i,
        abwesenheit_tage TYPE i,
        beschaeftigungstage TYPE i,
        anrechenbar     TYPE i,
        gewichtet TYPE decfloat34,
        bsgrd_durchschnitt TYPE decfloat34,
        bsgrd_fehlende_tage TYPE i,
        faktor          TYPE decfloat34,
        faktor_gueltig  TYPE abap_bool,
      END OF ty_month,
      ty_t_month TYPE SORTED TABLE OF ty_month WITH UNIQUE KEY monat.

    " Tagesgueltiger Beschaeftigungsgrad, ohne pauschale 100%-Annahme.
    CLASS-METHODS get_employment_percent
      IMPORTING
        it_pay TYPE ty_t_pay
        iv_date TYPE d
      EXPORTING
        ev_percent TYPE p0008-bsgrd
        ev_valid TYPE abap_bool.

    " Monatsfaktor aus tagesgenau gewichteten anrechenbaren Arbeitstagen.
    CLASS-METHODS summarize_days
      IMPORTING it_days TYPE ty_t_day
      RETURNING VALUE(rt_months) TYPE ty_t_month.

    CLASS-METHODS get_employment_reference
      IMPORTING
        it_actions TYPE ty_t_actions
        iv_date TYPE d
      EXPORTING
        ev_reference TYPE d
        ev_employed TYPE abap_bool
      EXCEPTIONS
        missing_employment.

    " REF01 leer: unbezahlt. Zeitraum ist die Schnittmenge
    " der Gueltigkeiten von T554S, T554C und dem Aufrufzeitraum.
    CLASS-METHODS get_unpaid_awart
      IMPORTING
        iv_molga TYPE t554c-molga
        iv_moabw TYPE t554s-moabw
        iv_begda TYPE begda
        iv_endda TYPE endda
        iv_modif TYPE t554c-modif DEFAULT '01'
      RETURNING
        VALUE(rt_unpaid) TYPE ty_t_unpaid.

    CLASS-METHODS get_factors
      IMPORTING
        iv_pernr        TYPE pernr_d
        iv_begda        TYPE begda
        iv_endda        TYPE endda
        it_unpaid_awart TYPE ty_t_awart OPTIONAL
        iv_modif        TYPE t554c-modif DEFAULT '01'
      EXPORTING
        et_months       TYPE ty_t_month
        et_days         TYPE ty_t_day
      EXCEPTIONS
        invalid_input
        infotype_error
        schedule_error
        customizing_error.

protected section.
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



CLASS ZCL_HR_BONUS_FACTOR IMPLEMENTATION.


  METHOD get_factors.
    DATA:
      lt_0000    TYPE STANDARD TABLE OF p0000,
      lt_0001    TYPE STANDARD TABLE OF p0001,
      lt_0002    TYPE STANDARD TABLE OF p0002,
      lt_0007    TYPE STANDARD TABLE OF p0007,
      lt_0008    TYPE ty_t_pay,
      lt_2001    TYPE STANDARD TABLE OF p2001,
      lt_2003    TYPE STANDARD TABLE OF p2003,
      lt_no_abs  TYPE STANDARD TABLE OF p2001,
      lt_no_att  TYPE STANDARD TABLE OF p2002,
      lt_perws   TYPE STANDARD TABLE OF ptpsp,
      lt_days    TYPE ty_t_day,
      lt_months  TYPE ty_t_month,
      lt_unpaid  TYPE ty_t_unpaid,
      lv_date    TYPE d,
      lv_warning TYPE sy-subrc,
      lv_read_begda TYPE begda VALUE '18000101',
      lv_read_endda TYPE endda VALUE '99991231',
      lt_plan_0000 TYPE STANDARD TABLE OF p0000,
      lt_plan_0001 TYPE STANDARD TABLE OF p0001,
      lt_plan_0002 TYPE STANDARD TABLE OF p0002,
      lt_plan_0007 TYPE STANDARD TABLE OF p0007,
      lv_reference TYPE d,
      lv_employed TYPE abap_bool.

    CLEAR: et_months, et_days.
    IF iv_pernr IS INITIAL
       OR iv_begda IS INITIAL OR iv_endda IS INITIAL
       OR iv_begda > iv_endda
       OR iv_begda(4) <> iv_endda(4)
       OR iv_modif IS INITIAL.
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
          iv_begda = lv_read_begda
          iv_endda = lv_read_endda
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
    " Historie wird fuer die Fortschreibung vor Eintritt/nach Austritt
    " gebraucht. Bewegungsdaten nur fuer den angeforderten Zeitraum.
    lv_read_begda = iv_begda.
    lv_read_endda = iv_endda.
    read_it '0008' lt_0008.
    read_it '2001' lt_2001.
    read_it '2003' lt_2003.

    " Historische organisatorische Zuordnung beachten: ein Wechsel
    " darf nicht die Bewertung des gesamten Jahres veraendern.
    LOOP AT lt_0001 INTO DATA(ls_org)
      WHERE begda <= iv_endda AND endda >= iv_begda.
      SELECT SINGLE molga, moabw
        FROM t001p
        WHERE werks = @ls_org-werks AND btrtl = @ls_org-btrtl
        INTO @DATA(ls_group).
      IF sy-subrc <> 0.
        RAISE customizing_error.
      ENDIF.

      DATA(lv_from) = iv_begda.
      DATA(lv_to) = iv_endda.
      IF ls_org-begda > lv_from.
        lv_from = ls_org-begda.
      ENDIF.
      IF ls_org-endda < lv_to.
        lv_to = ls_org-endda.
      ENDIF.

      DATA(lt_org_unpaid) = get_unpaid_awart(
        iv_molga = ls_group-molga
        iv_moabw = ls_group-moabw
        iv_modif = iv_modif
        iv_begda = lv_from
        iv_endda = lv_to ).
      INSERT LINES OF lt_org_unpaid INTO TABLE lt_unpaid.
    ENDLOOP.

    TYPES: BEGIN OF ty_day_group,
             datum TYPE d,
             moabw TYPE t001p-moabw,
           END OF ty_day_group.
    DATA lt_day_group TYPE SORTED TABLE OF ty_day_group WITH UNIQUE KEY datum.
    SELECT * FROM t554t WHERE sprsl = @sy-langu
      INTO TABLE @DATA(lt_abs_texts).

    " Nur lokale Kopien fuer die Planerzeugung. Keine Stammdatenpflege!
    " Vor dem ersten Eintritt: erste Beschaeftigungsregel.
    " Nach Austritt/in Wiedereintrittsluecken: letzte Beschaeftigungsregel.
    " SAP erzeugt damit den Plan am Zieldatum (Schichtzyklus/Feiertage).
    lv_date = iv_begda.
    DO.
      get_employment_reference(
        EXPORTING it_actions = lt_0000 iv_date = lv_date
        IMPORTING ev_reference = lv_reference ev_employed = lv_employed
        EXCEPTIONS missing_employment = 1 ).
      IF sy-subrc <> 0.
        RAISE infotype_error.
      ENDIF.

      LOOP AT lt_0000 INTO DATA(ls_action)
        WHERE begda <= lv_reference AND endda >= lv_reference
          AND ( stat2 = '1' OR stat2 = '3' ).
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        RAISE infotype_error.
      ENDIF.
      ls_action-begda = lv_date.
      ls_action-endda = lv_date.
      ls_action-stat2 = '3'. " Ungekuerzte Sollbasis
      APPEND ls_action TO lt_plan_0000.

      LOOP AT lt_0001 INTO DATA(ls_plan_org)
        WHERE begda <= lv_reference AND endda >= lv_reference.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        RAISE schedule_error.
      ENDIF.
      SELECT SINGLE moabw FROM t001p
        WHERE werks = @ls_plan_org-werks AND btrtl = @ls_plan_org-btrtl
        INTO @DATA(lv_moabw).
      IF sy-subrc <> 0.
        RAISE customizing_error.
      ENDIF.
      INSERT VALUE #( datum = lv_date moabw = lv_moabw )
        INTO TABLE lt_day_group.
      ls_plan_org-begda = lv_date.
      ls_plan_org-endda = lv_date.
      APPEND ls_plan_org TO lt_plan_0001.

      LOOP AT lt_0007 INTO DATA(ls_plan_time)
        WHERE begda <= lv_reference AND endda >= lv_reference.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        RAISE schedule_error.
      ENDIF.
      ls_plan_time-begda = lv_date.
      ls_plan_time-endda = lv_date.
      APPEND ls_plan_time TO lt_plan_0007.

      LOOP AT lt_0002 INTO DATA(ls_plan_person)
        WHERE begda <= lv_reference AND endda >= lv_reference.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        ls_plan_person-begda = lv_date.
        ls_plan_person-endda = lv_date.
        APPEND ls_plan_person TO lt_plan_0002.
      ENDIF.

      INSERT VALUE #( datum = lv_date beschaeftigt = lv_employed )
        INTO TABLE lt_days.
      IF lv_date = iv_endda.
        EXIT.
      ENDIF.
      lv_date = lv_date + 1.
    ENDDO.

    " Vertretungen nur an realen Beschaeftigungstagen verwenden.
    DATA lt_plan_2003 TYPE STANDARD TABLE OF p2003.
    LOOP AT lt_2003 INTO DATA(ls_substitution).
      LOOP AT lt_days INTO DATA(ls_employment)
        WHERE datum >= ls_substitution-begda
          AND datum <= ls_substitution-endda
          AND beschaeftigt = abap_true.
        DATA(ls_plan_substitution) = ls_substitution.
        ls_plan_substitution-begda = ls_employment-datum.
        ls_plan_substitution-endda = ls_employment-datum.
        APPEND ls_plan_substitution TO lt_plan_2003.
      ENDLOOP.
    ENDLOOP.

    " Sollbasis ohne Abwesenheiten; reale Vertretungen bleiben erhalten.
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
        i0000           = lt_plan_0000
        i0001           = lt_plan_0001
        i0002           = lt_plan_0002
        i0007           = lt_plan_0007
        i2001           = lt_no_abs
        i2002           = lt_no_att
        i2003           = lt_plan_2003
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
      " Eine explizite Liste ist nur ein zusaetzlicher Filter.
      " Sie kann keine laut REF01 bezahlten Arten einschliessen.
      IF it_unpaid_awart IS NOT INITIAL.
        READ TABLE it_unpaid_awart
          WITH TABLE KEY table_line = <absence>-awart
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE lt_2001.
        ENDIF.
      ENDIF.
    ENDLOOP.

    lv_date = iv_begda.
    DO.
      READ TABLE lt_perws INTO DATA(ls_plan)
        WITH KEY datum = lv_date.
      IF sy-subrc <> 0.
        RAISE schedule_error.
      ENDIF.

      READ TABLE lt_days INTO DATA(ls_day)
        WITH TABLE KEY datum = lv_date.
      ls_day-tprog = ls_plan-tprog.
      ls_day-sollstunden = ls_plan-stdaz.
      IF ls_day-beschaeftigt = abap_true.
        get_employment_percent(
          EXPORTING it_pay = lt_0008 iv_date = lv_date
          IMPORTING ev_percent = ls_day-bsgrd
                    ev_valid = ls_day-bsgrd_gueltig ).
      ENDIF.

      " Alle passenden ganzen Abwesenheiten anzeigen, auch an freien Tagen.
      " Mehrere Arten werden eindeutig aufgelistet, Tage nie vervielfacht.
      DATA lt_day_awart TYPE ty_t_awart.
      CLEAR lt_day_awart.
      READ TABLE lt_day_group INTO DATA(ls_day_group)
        WITH TABLE KEY datum = lv_date.
      LOOP AT lt_2001 INTO DATA(ls_absence)
        WHERE begda <= lv_date AND endda >= lv_date.
        LOOP AT lt_unpaid TRANSPORTING NO FIELDS
          WHERE awart = ls_absence-awart
            AND begda <= lv_date AND endda >= lv_date.
          INSERT ls_absence-awart INTO TABLE lt_day_awart.
          EXIT.
        ENDLOOP.
      ENDLOOP.
      LOOP AT lt_day_awart INTO DATA(lv_awart).
        READ TABLE lt_abs_texts INTO DATA(ls_abs_text)
          WITH KEY moabw = ls_day_group-moabw awart = lv_awart.
        DATA(lv_abs_text) = CONV string( 'Text nicht gepflegt' ).
        IF sy-subrc = 0.
          lv_abs_text = ls_abs_text-atext.
        ENDIF.
        IF ls_day-awart IS INITIAL.
          ls_day-awart = lv_awart.
          ls_day-atext = |{ lv_awart }: { lv_abs_text }|.
        ELSE.
          ls_day-awart = |{ ls_day-awart }; { lv_awart }|.
          ls_day-atext = |{ ls_day-atext }; { lv_awart }: { lv_abs_text }|.
        ENDIF.
      ENDLOOP.

      IF ls_plan-stdaz > 0.
        ls_day-solltag = 1.
        IF ls_day-beschaeftigt = abap_false.
          ls_day-ausserhalb_tag = 1.
          ls_day-unbezahlt_tag = 1.
        ELSEIF lt_day_awart IS NOT INITIAL.
          ls_day-unbezahlt_tag = 1.
          ls_day-abwesenheit_tag = 1.
        ENDIF.
      ENDIF.
      MODIFY TABLE lt_days FROM ls_day.

      IF lv_date = iv_endda.
        EXIT.
      ENDIF.
      lv_date = lv_date + 1.
    ENDDO.

    lt_months = summarize_days( lt_days ).
    et_days = lt_days.
    et_months = lt_months.
  ENDMETHOD.



  METHOD summarize_days.
    DATA lv_month TYPE n LENGTH 6.
    LOOP AT it_days INTO DATA(ls_day).
      lv_month = ls_day-datum(6).
      READ TABLE rt_months ASSIGNING FIELD-SYMBOL(<month>)
        WITH TABLE KEY monat = lv_month.
      IF sy-subrc <> 0.
        INSERT VALUE #( monat = lv_month )
          INTO TABLE rt_months ASSIGNING <month>.
      ENDIF.
      <month>-solltage = <month>-solltage + ls_day-solltag.
      <month>-unbezahlte_tage =
        <month>-unbezahlte_tage + ls_day-unbezahlt_tag.
      <month>-ausserhalb_tage =
        <month>-ausserhalb_tage + ls_day-ausserhalb_tag.
      <month>-abwesenheit_tage =
        <month>-abwesenheit_tage + ls_day-abwesenheit_tag.
      IF ls_day-beschaeftigt = abap_true.
        <month>-beschaeftigungstage = <month>-beschaeftigungstage + 1.
      ENDIF.
      " Unbezahlte/ausserhalb liegende Tage tragen immer null bei.
      IF ls_day-solltag = 1 AND ls_day-unbezahlt_tag = 0.
        IF ls_day-bsgrd_gueltig = abap_true.
          <month>-gewichtet = <month>-gewichtet
            + CONV decfloat34( ls_day-bsgrd ) / 100.
        ELSE.
          <month>-bsgrd_fehlende_tage = <month>-bsgrd_fehlende_tage + 1.
        ENDIF.
      ENDIF.

    ENDLOOP.
    LOOP AT rt_months ASSIGNING <month>.
      <month>-anrechenbar =
        <month>-solltage - <month>-unbezahlte_tage.
      IF <month>-bsgrd_fehlende_tage > 0.
        " Keine Teilsumme als vollstaendiges Ergebnis ausgeben.
        CLEAR: <month>-gewichtet, <month>-bsgrd_durchschnitt,
               <month>-faktor, <month>-faktor_gueltig.
      ELSEIF <month>-solltage > 0.
        <month>-faktor = <month>-gewichtet
                        / CONV decfloat34( <month>-solltage ).
        IF <month>-anrechenbar > 0.
          <month>-bsgrd_durchschnitt = <month>-gewichtet * 100
            / CONV decfloat34( <month>-anrechenbar ).
        ENDIF.
        <month>-faktor_gueltig = abap_true.
      ELSEIF <month>-beschaeftigungstage = 0.
        " Ganzer Monat ohne Beschaeftigung: auch bei 0 Solltagen Faktor 0.
        <month>-faktor = 0.
        <month>-faktor_gueltig = abap_true.
      ELSE.
        " Ohne Solltage waehrend Beschaeftigung ist kein Quotient definiert.
        CLEAR: <month>-faktor, <month>-faktor_gueltig.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_employment_percent.
    CLEAR: ev_percent, ev_valid.
    LOOP AT it_pay INTO DATA(ls_pay)
      WHERE begda <= iv_date AND endda >= iv_date AND sprps = space.
      IF ls_pay-bsgrd < 0.
        CLEAR: ev_percent, ev_valid.
        RETURN.
      ENDIF.
      " Mehrere gueltige Saetze duerfen keinen widerspruechlichen Grad haben.
      IF ev_valid = abap_true AND ev_percent <> ls_pay-bsgrd.
        CLEAR: ev_percent, ev_valid.
        RETURN.
      ENDIF.
      ev_percent = ls_pay-bsgrd.
      ev_valid = abap_true.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_employment_reference.
    DATA: lv_previous TYPE d, lv_next TYPE d.
    CLEAR: ev_reference, ev_employed.
    LOOP AT it_actions INTO DATA(ls_action)
      WHERE sprps = space AND ( stat2 = '1' OR stat2 = '3' ).
      IF ls_action-begda <= iv_date AND ls_action-endda >= iv_date.
        ev_reference = iv_date.
        ev_employed = abap_true.
        RETURN.
      ENDIF.
      IF ls_action-endda < iv_date AND ls_action-endda > lv_previous.
        lv_previous = ls_action-endda.
      ENDIF.
      IF ls_action-begda > iv_date.
        IF lv_next IS INITIAL OR ls_action-begda < lv_next.
          lv_next = ls_action-begda.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF lv_previous IS NOT INITIAL.
      ev_reference = lv_previous.
    ELSEIF lv_next IS NOT INITIAL.
      ev_reference = lv_next.
    ELSE.
      " Kein belegtes Arbeitsverhaeltnis: keine erfundene Planbasis.
      RAISE missing_employment.
    ENDIF.
  ENDMETHOD.

  METHOD get_unpaid_awart.
    " KLBEW verbindet Abwesenheitsart und Bewertungsregel.
    " MOABW und MODIF sind unabhaengige Gruppierungen!
    " OCABS leer: regulaere Bewertung, keine Offcycle-Variante.
    SELECT s~subty AS awart,
           s~begda AS s_begda, s~endda AS s_endda,
           c~begda AS c_begda, c~endda AS c_endda
      FROM t554s AS s
      INNER JOIN t554c AS c ON c~klbew = s~klbew
      WHERE s~moabw = @iv_moabw
        AND s~begda <= @iv_endda
        AND s~endda >= @iv_begda
        AND c~molga = @iv_molga
        AND c~modif = @iv_modif
        AND c~ocabs = @space
        AND c~ref01 = @space
        AND c~begda <= @iv_endda
        AND c~endda >= @iv_begda
        AND c~begda <= s~endda
        AND c~endda >= s~begda
      INTO TABLE @DATA(lt_rules).

    LOOP AT lt_rules INTO DATA(ls_rule).
      DATA(ls_unpaid) = VALUE ty_unpaid(
        awart = ls_rule-awart begda = iv_begda endda = iv_endda ).
      IF ls_rule-s_begda > ls_unpaid-begda.
        ls_unpaid-begda = ls_rule-s_begda.
      ENDIF.
      IF ls_rule-c_begda > ls_unpaid-begda.
        ls_unpaid-begda = ls_rule-c_begda.
      ENDIF.
      IF ls_rule-s_endda < ls_unpaid-endda.
        ls_unpaid-endda = ls_rule-s_endda.
      ENDIF.
      IF ls_rule-c_endda < ls_unpaid-endda.
        ls_unpaid-endda = ls_rule-c_endda.
      ENDIF.
      IF ls_unpaid-begda <= ls_unpaid-endda.
        INSERT ls_unpaid INTO TABLE rt_unpaid.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


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

    " Ein fehlender Infotyp ist eine leere Ergebnismenge.
    " Sonstige Fehler duerfen nicht als fehlende Daten gelten.
    CASE sy-subrc.
      WHEN 1. " INFTY_NOT_FOUND
        CLEAR ct_data.
        RETURN.
      WHEN 0.
*        IF lv_subrc = 4.
**          CLEAR ct_data.
*          RETURN.
*        ELSEIF lv_subrc <> 0.
*          RAISE read_error.
*        ENDIF.
      WHEN OTHERS.
*        RAISE read_error.
    ENDCASE.
    " Ob die Daten fuer einen Arbeitszeitplan ausreichen,
    " entscheidet anschliessend HR_PERSONAL_WORK_SCHEDULE.

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
ENDCLASS.
