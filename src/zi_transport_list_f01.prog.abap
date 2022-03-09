*&---------------------------------------------------------------------*
*&  Include           ZI_TRANSPORT_LIST_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Subroutines
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&   Form TRANSPORTS_SELECTION
*&---------------------------------------------------------------------*
*    Select transports based on the selection
*----------------------------------------------------------------------*
FORM transports_selection .

  SELECT
   e070~trkorr AS transport
   e070~trfunction AS trfunction
   e070~trstatus AS status
   e070~tarsystem AS target
   e070~korrdev AS category
   e070~as4user AS owner
   e070~as4date AS chg_date
   e070~as4time AS chg_time
   e070c~client AS client
   e07t~as4text AS desc
   INTO CORRESPONDING FIELDS OF TABLE gt_transport_list
   FROM e070
    INNER JOIN e070c ON e070~trkorr EQ e070c~trkorr
    INNER JOIN e07t ON e070~trkorr EQ e07t~trkorr
   WHERE e070~trkorr IN s_trans
    AND e070~trfunction IN ('K','W')
    AND e070~trstatus IN s_status
    AND e070~tarsystem IN s_target
    AND e070~korrdev IN s_catgry
    AND e070c~client IN s_client
    AND e070~as4user IN s_owner
    AND e070~as4date IN s_chg_dt
    AND e07t~as4text IN s_desc
    .


ENDFORM.          " TRANSPORTS_SELECTION
*&---------------------------------------------------------------------*
*&   Form TASKS_SELECTION
*&---------------------------------------------------------------------*
*    Select tasks if option is selected
*----------------------------------------------------------------------*
FORM tasks_selection .

  SELECT
   strkorr
   INTO TABLE gt_task_list
   FROM e070
   WHERE e070~trkorr IN s_tasks
    AND e070~trfunction IN ('Q','S','R','X')
    AND e070~trstatus IN s_stat2
    AND e070~as4user IN s_owner2
    AND e070~as4date IN s_chg_d2
    .

ENDFORM.          " TASKS_SELECTION

*&---------------------------------------------------------------------*
*&   Form OBJ_TRANSPORT_SELECTION
*&---------------------------------------------------------------------*
*    Select transports based on objects
*----------------------------------------------------------------------*
FORM obj_transport_selection .

  SELECT
   e071~trkorr AS transport
   INTO TABLE gt_transport_obj
   FROM e071
   FOR ALL ENTRIES IN gt_transport_list
   WHERE e071~trkorr EQ gt_transport_list-transport
    AND e071~pgmid IN s_pgmid
    AND e071~object IN s_object
    AND e071~obj_name IN s_objnam
    .

  SELECT
   e071k~trkorr AS transport
   APPENDING TABLE gt_transport_obj
   FROM e071k
   FOR ALL ENTRIES IN gt_transport_list
   WHERE e071k~trkorr EQ gt_transport_list-transport
    AND e071k~pgmid IN s_pgmid
    AND e071k~object IN s_object
    AND e071k~objname IN s_objnam
    AND e071k~tabkey IN s_tabkey
    .

  SORT gt_transport_obj.
  DELETE ADJACENT DUPLICATES FROM gt_transport_obj.
  LOOP AT gt_transport_list INTO gw_transport.
    READ TABLE gt_transport_obj
     INTO gw_task
     WITH KEY transport = gw_transport-transport
     BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE gt_transport_list.
    ENDIF.
  ENDLOOP.

ENDFORM.          " OBJ_TRANSPORT_SELECTION


*&---------------------------------------------------------------------*
*&   Form DEP_TRANSPORT_SELECTION
*&---------------------------------------------------------------------*
*    Select transports based on objects
*----------------------------------------------------------------------*
FORM dep_transport_selection .

  IF gt_transport_list IS NOT INITIAL.
    SELECT
     e071~pgmid
     e071~object
     e071~obj_name
     INTO TABLE gt_objects
     FROM e071
     FOR ALL ENTRIES IN gt_transport_list
     WHERE e071~trkorr EQ gt_transport_list-transport
       AND e071~pgmid IN ('LANG','LIMU','R3TR')
       AND e071~object NOT IN ('VDAT','CDAT','TABU','TDAT')
      .
  ENDIF.

  SORT gt_objects
    BY
      pgmid
      object
      obj_name.
  DELETE ADJACENT DUPLICATES FROM gt_objects
    COMPARING ALL FIELDS.

  LOOP AT gt_objects INTO gw_objects.
    IF gw_objects-pgmid = 'LIMU'.
      PERFORM get_r3tr_object
        TABLES
          gt_objects_drv
        USING
          gw_objects-object
          gw_objects-obj_name
          gw_objects-deep.

      PERFORM get_limu_objects
        TABLES
          gt_objects_drv
        USING
          gw_objects-pgmid
          gw_objects-object
          gw_objects-obj_name
          gw_objects-deep.

    ELSEIF gw_objects-pgmid = 'R3TR'.
      PERFORM get_limu_objects
        TABLES
          gt_objects_drv
        USING
          gw_objects-pgmid
          gw_objects-object
          gw_objects-obj_name
          gw_objects-deep.
    ENDIF.
  ENDLOOP.

*** Begin of changes
*** Get dependent objects
  PERFORM get_dependent_objects
    TABLES  gt_objects_drv.
*** End of changes

  APPEND LINES OF gt_objects_drv TO gt_objects.
  SORT gt_objects
    BY
      pgmid
      object
      obj_name.
  DELETE ADJACENT DUPLICATES FROM gt_objects
    COMPARING ALL FIELDS.

*** Begin of changes
  LOOP AT gt_objects INTO gw_objects.
    IF gw_objects-pgmid = 'LIMU'.
      PERFORM get_r3tr_object
        TABLES
          gt_objects_drv
        USING
          gw_objects-object
          gw_objects-obj_name
          gw_objects-deep.

      PERFORM get_limu_objects
        TABLES
          gt_objects_drv
        USING
          gw_objects-pgmid
          gw_objects-object
          gw_objects-obj_name
          gw_objects-deep.

    ELSEIF gw_objects-pgmid = 'R3TR'.
      PERFORM get_limu_objects
        TABLES
          gt_objects_drv
        USING
          gw_objects-pgmid
          gw_objects-object
          gw_objects-obj_name
          gw_objects-deep.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF gt_objects_drv TO gt_objects.
  SORT gt_objects
    BY
      pgmid
      object
      obj_name.
  DELETE ADJACENT DUPLICATES FROM gt_objects
    COMPARING ALL FIELDS.
*** End of changes

  IF gt_objects IS NOT INITIAL.
    SELECT
     trkorr AS transport
     pgmid
     object
     obj_name
     INTO TABLE gt_transport_dep
     FROM e071
     FOR ALL ENTRIES IN gt_objects
     WHERE pgmid EQ gt_objects-pgmid
        AND object EQ gt_objects-object
        AND obj_name EQ gt_objects-obj_name
      .
  ENDIF.

*** Begin of changes
  LOOP AT gt_transport_dep ASSIGNING FIELD-SYMBOL(<lw_transport_dep>).
    READ TABLE gt_objects INTO DATA(lw_data)
      WITH KEY  pgmid     = <lw_transport_dep>-pgmid
                object    = <lw_transport_dep>-object
                obj_name  = <lw_transport_dep>-obj_name.
    IF sy-subrc EQ 0.
      <lw_transport_dep>-deep = lw_data-deep.
    ENDIF.
  ENDLOOP.
*** End of changes

  IF gt_transport_list IS NOT INITIAL.
    SELECT
     pgmid
     object
     objname
      tabkey
     INTO TABLE gt_tab_objects
     FROM e071k
     FOR ALL ENTRIES IN gt_transport_list
     WHERE trkorr EQ gt_transport_list-transport
      .
  ENDIF.

  SORT gt_tab_objects
    BY
      pgmid
      object
      obj_name
      tabkey.
  DELETE ADJACENT DUPLICATES FROM gt_tab_objects
    COMPARING ALL FIELDS.

  IF gt_tab_objects IS NOT INITIAL.
    SELECT
     trkorr AS transport
     APPENDING TABLE gt_transport_dep
     FROM e071k
     FOR ALL ENTRIES IN gt_tab_objects
     WHERE pgmid EQ gt_tab_objects-pgmid
        AND object EQ gt_tab_objects-object
        AND objname EQ gt_tab_objects-obj_name
        AND tabkey EQ gt_tab_objects-tabkey
      .
  ENDIF.

  SORT gt_transport_dep.
  DELETE ADJACENT DUPLICATES FROM gt_transport_dep.

  SORT gt_transport_list BY transport.
  LOOP AT gt_transport_dep INTO gw_task.
    READ TABLE gt_transport_list
     INTO gw_transport
     WITH KEY transport = gw_task-transport
     BINARY SEARCH.
    IF sy-subrc EQ 0.
      DELETE gt_transport_dep.
    ENDIF.
  ENDLOOP.

  IF gt_transport_dep IS NOT INITIAL.
    SELECT
      e070~trkorr AS transport
      e070~trfunction AS trfunction
      e070~trstatus AS status
      e070~tarsystem AS target
      e070~korrdev AS category
      e070~as4user AS owner
      e070~as4date AS chg_date
      e070~as4time AS chg_time
      e070c~client AS client
      e07t~as4text AS desc
      APPENDING CORRESPONDING FIELDS OF TABLE gt_transport_list
      FROM e070
        INNER JOIN e070c ON e070~trkorr EQ e070c~trkorr
        INNER JOIN e07t ON e070~trkorr EQ e07t~trkorr
      FOR ALL ENTRIES IN gt_transport_dep
      WHERE e070~trkorr EQ gt_transport_dep-transport
        AND e070~trkorr IN s_trn_dp
        AND e070~trstatus IN s_sta_dp
        AND e070~as4date IN s_cdt_dt.
*        AND e070~trfunction IN ('K','W')
    .
  ENDIF.

  SORT gt_transport_list
    BY transport.

  LOOP AT gt_transport_list INTO gw_transport.
    READ TABLE gt_transport_dep
     INTO gw_task
     WITH KEY transport = gw_transport-transport
     BINARY SEARCH.
    IF sy-subrc EQ 0.
      gw_transport-deep     = gw_task-deep.
      gw_transport-dep_stat = gc_icon_related.
      MODIFY gt_transport_list FROM gw_transport.
    ENDIF.
  ENDLOOP.

*** Begin of changes
  DELETE gt_transport_list WHERE ( deep EQ 0 AND ( trfunction NE 'K' AND trfunction NE 'W' ) AND status EQ 'R' ).

  DELETE gt_transport_list WHERE ( deep NE 0 AND ( trfunction NE 'K' AND trfunction NE 'W' ) AND status EQ 'R' ).

  SORT gt_transport_list
    BY  deep DESCENDING
        transport ASCENDING.
*** End of changes

ENDFORM.          " DEP_TRANSPORT_SELECTION

*&---------------------------------------------------------------------*
*&   Form ADDL_TRANSPORT_SELECTION
*&---------------------------------------------------------------------*
*    Selection for additional transports for selected tasks
*----------------------------------------------------------------------*
FORM addl_transport_selection .

  SELECT
   e070~trkorr AS transport
   e070~trfunction AS trfunction
   e070~trstatus AS status
   e070~tarsystem AS target
   e070~korrdev AS category
   e070~as4user AS owner
   e070~as4date AS chg_date
   e070~as4time AS chg_time
   e070c~client AS client
   e07t~as4text AS desc
   APPENDING CORRESPONDING FIELDS OF TABLE gt_transport_list
   FROM e070
    INNER JOIN e070c ON e070~trkorr EQ e070c~trkorr
    INNER JOIN e07t ON e070~trkorr EQ e07t~trkorr
   FOR ALL ENTRIES IN gt_task_list
   WHERE e070~trkorr EQ gt_task_list-transport
    AND e070~trfunction IN ('K','W')
    .

  SORT gt_transport_list BY transport.
  DELETE ADJACENT DUPLICATES
   FROM gt_transport_list
   COMPARING transport.

ENDFORM.          " ADDL_TRANSPORT_SELECTION
*&---------------------------------------------------------------------*
*&   Form SUBS_TRANSPORT_SELECTION
*&---------------------------------------------------------------------*
*    Filter transport list only for tasks selected
*----------------------------------------------------------------------*
FORM subs_transport_selection .

  LOOP AT gt_transport_list INTO gw_transport.
    READ TABLE gt_task_list
     INTO gw_task
     WITH KEY transport = gw_transport-transport.
    IF sy-subrc NE 0.
      DELETE gt_transport_list.
    ENDIF.
  ENDLOOP.

ENDFORM.          " SUBS_TRANSPORT_SELECTION
*&---------------------------------------------------------------------*
*&   Form RELEASE_DATA_SELECTION
*&---------------------------------------------------------------------*
*    Select the release data from the transport log for the
*  transports in the list
*----------------------------------------------------------------------*
FORM release_data_selection .

  DATA:
    lv_trkorr     LIKE e070-trkorr, "temporary variable for transport
*  Transport logs
    lw_settings   TYPE ctslg_settings,
    lw_cofile     TYPE ctslg_cofile,
    lv_qa_client  TYPE ctslg_step-clientid,
    lv_qa2_client TYPE ctslg_step-clientid,
    lv_qa3_client TYPE ctslg_step-clientid,
    lv_prd_client TYPE ctslg_step-clientid.

  LOOP AT gt_transport_list INTO gw_transport.
    IF gw_transport-status NE 'R'.
      SELECT SINGLE
       trkorr
       INTO lv_trkorr
       FROM e070
       WHERE strkorr EQ gw_transport-transport
        AND trstatus NE 'R'
        .
      IF sy-subrc EQ 0. "Open tasks exist
        gw_transport-dev_stat = gc_icon_edit.
      ELSE. "No open tasks exist
        gw_transport-dev_stat = gc_icon_ready.
      ENDIF.
*      gw_transport-qa_stat = gc_icon_no_logdata.
      gw_transport-qa2_stat = gc_icon_no_logdata.
*      gw_transport-qa3_stat = gc_icon_no_logdata.
      gw_transport-prd_stat = gc_icon_no_logdata.
    ELSE.

      IF gw_transport-category = 'SYST'. "Workbench
        CLEAR: lv_qa_client, lv_qa2_client, lv_qa3_client, lv_prd_client.
      ELSE.
*        lv_qa_client = p_qa_mn.
        lv_qa2_client = p_qa2_mn.
*        lv_qa3_client = p_qa3_mn.
        lv_prd_client = p_prd_mn.
      ENDIF.

      lw_settings-detailed_depiction = 'X'.
      CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
        EXPORTING
          iv_trkorr   = gw_transport-transport
          is_settings = lw_settings
        IMPORTING
          es_cofile   = lw_cofile
        EXCEPTIONS
          OTHERS      = 1.
      IF lw_cofile-imported EQ 'X'.
        PERFORM import_status_determination
         USING lw_cofile p_dev space 'E' gw_transport-category gw_transport-transport
         CHANGING
          gw_transport-dev_stat
          gw_transport-rel_date
          gw_transport-rel_time
          gw_transport-rel_rc.
*        PERFORM import_status_determination
*         USING lw_cofile p_qa lv_qa_client 'I' gw_transport-category gw_transport-transport
*         CHANGING
*          gw_transport-qa_stat
*          gw_transport-qa_date
*          gw_transport-qa_time
*          gw_transport-qa_rc.
        PERFORM import_status_determination
         USING lw_cofile p_qa2 lv_qa2_client 'I' gw_transport-category gw_transport-transport
         CHANGING
          gw_transport-qa2_stat
          gw_transport-qa2_date
          gw_transport-qa2_time
          gw_transport-qa2_rc.    "gw_transport-qa3_rc.
*        PERFORM import_status_determination
*         USING lw_cofile p_qa3 lv_qa3_client 'I' gw_transport-category gw_transport-transport
*         CHANGING
*          gw_transport-qa3_stat
*          gw_transport-qa3_date
*          gw_transport-qa3_time
*          gw_transport-qa3_rc.
        PERFORM import_status_determination
         USING lw_cofile p_prd lv_prd_client 'I' gw_transport-category gw_transport-transport
         CHANGING
          gw_transport-prd_stat
          gw_transport-prd_date
          gw_transport-prd_time
          gw_transport-prd_rc.
      ELSE.
        PERFORM import_status_determination
         USING lw_cofile p_dev space 'E' gw_transport-category gw_transport-transport
         CHANGING
          gw_transport-dev_stat
          gw_transport-rel_date
          gw_transport-rel_time
          gw_transport-rel_rc.
*        gw_transport-qa_stat = gc_icon_no_logdata.
        gw_transport-qa2_stat = gc_icon_no_logdata.
*        gw_transport-qa3_stat = gc_icon_no_logdata.
        gw_transport-prd_stat = gc_icon_no_logdata.
      ENDIF.
    ENDIF.
*    select single
*      trkorr
*      into lv_trkorr
*      from zbc_trans_abnd
*      where trkorr eq gw_transport-transport
*        .
*    if sy-subrc eq 0.
*      gw_transport-abd_stat = gc_icon_abandoned.
*    endif.
    MODIFY gt_transport_list FROM gw_transport.
  ENDLOOP.

ENDFORM.          " RELEASE_DATA_SELECTION
*&---------------------------------------------------------------------*
*&   Form LIST_DISPLAY
*&---------------------------------------------------------------------*
*    Generate ALV field catalog and output transport list
*----------------------------------------------------------------------*
FORM list_display .

  PERFORM layout_setup.

  PERFORM fieldcat_setup.

  PERFORM alv_display.

ENDFORM.          " LIST_DISPLAY
*&---------------------------------------------------------------------*
*&   Form SELECTION_DEFAULTS
*&---------------------------------------------------------------------*
*    Default selection screen inputs
*----------------------------------------------------------------------*
FORM selection_defaults .

  CLEAR s_trans.
  s_trans-sign = 'I'.
  s_trans-option = 'CP'.
  s_trans-low = sy-sysid && |K*|.   "'ERDK*'.
  APPEND s_trans.

  CLEAR s_trn_dp.
  s_trn_dp-sign = 'I'.
  s_trn_dp-option = 'CP'.
  s_trn_dp-low = sy-sysid && |K*|.  "'ERDK*'.
  APPEND s_trn_dp.

  CLEAR s_tasks.
  s_tasks-sign = 'I'.
  s_tasks-option = 'CP'.
  s_tasks-low = sy-sysid && |K*|.   "'ERDK*'.
  APPEND s_tasks.

  CLEAR s_owner.
  s_owner-sign = 'I'.
  s_owner-option = 'CP'.
  s_owner-low = sy-uname.
  APPEND s_owner.

  CLEAR s_owner2.
  s_owner2-sign = 'I'.
  s_owner2-option = 'CP'.
  s_owner2-low = sy-uname.
  APPEND s_owner2.

* Dev system
  p_dev = 'E2T'.    "'ERD'.
*  p_qa = 'E2T'.     "'ERD'.
*  p_qa_mn = 100.    "150.

* QA System
  p_qa2 = 'E2T'.     "'ERU'.
  p_qa2_mn = 100.    "220.

* Pre-prod system
*  p_qa3 = 'ERT'.
*  p_qa3_mn = 300.

* Prod system
  p_prd = 'E2T'.       "'ERP'.
  p_prd_mn = 100.   "300.

ENDFORM.          " SELECTION_DEFAULTS
*&---------------------------------------------------------------------*
*&   Form TRANSPORT_DISPLAY
*&---------------------------------------------------------------------*
*    Display a transport
*----------------------------------------------------------------------*
*   -->PV_TRANSPORT transport number to display
*----------------------------------------------------------------------*
FORM transport_display USING  pv_transport.

  DATA:
    lw_transport_number  TYPE cts_trkorr,
    lt_transport_numbers TYPE cts_trkorrs.

  lw_transport_number-trkorr = pv_transport.
  APPEND lw_transport_number TO lt_transport_numbers.

  CALL FUNCTION 'TR_DISPLAY_REQUESTS'
    EXPORTING
      it_request_numbers = lt_transport_numbers
    EXCEPTIONS
      OTHERS             = 1.


ENDFORM.          " TRANSPORT_DISPLAY
*&---------------------------------------------------------------------*
*&   Form IMPORT_STATUS_DETERMINATION
*&---------------------------------------------------------------------*
*    Dtermine the status, date, time and code of import/export
*----------------------------------------------------------------------*
*   -->PW_COFILE transport log
*   -->PV_SYSTEMID system
*   -->PV_CLIENTID client
*   -->PV_STEPID step id (I = import, E = export)
*   <--PV_STAT status icon
*   <--PV_DATE date of import/export
*   <--PV_TIME time of import/export
*   <--PV_RC return code for import/export
*----------------------------------------------------------------------*
FORM import_status_determination
USING
  pw_cofile TYPE ctslg_cofile
  pv_systemid TYPE ctslg_system-systemid
  pv_clientid TYPE ctslg_step-clientid
  pv_stepid TYPE ctslg_step-stepid
  pv_category TYPE e070-korrdev
  pv_transport TYPE e070-trkorr
CHANGING
  pv_stat TYPE icon-id
  pv_date TYPE ctslg_action-date
  pv_time TYPE ctslg_action-time
  pv_rc TYPE ctslg_action-rc
.

  DATA:
    lw_system  TYPE ctslg_system,
    lt_systems TYPE TABLE OF ctslg_system,
    lw_step    TYPE ctslg_step,
    lt_steps   TYPE TABLE OF ctslg_step,
    lw_action  TYPE ctslg_action,
    lt_actions TYPE TABLE OF ctslg_action,
    lv_index   TYPE i.

  lt_systems = pw_cofile-systems.
  READ TABLE lt_systems INTO lw_system
   WITH KEY systemid = pv_systemid.
  IF sy-subrc NE 0.
    pv_stat = gc_icon_no_logdata.
    PERFORM check_other_trn_group
      USING
        pv_transport
        pv_systemid
        pv_clientid
        pv_category
      CHANGING
        pv_stat
        pv_date
        pv_time
        pv_rc.
  ELSE.
    lt_steps = lw_system-steps.
    IF pv_stepid EQ 'I'.
      IF pv_category EQ 'SYST'.
        LOOP AT lt_steps INTO lw_step.
          IF lw_step-stepid EQ '<'.
            lv_index = sy-tabix.
          ENDIF.
        ENDLOOP.
        LOOP AT lt_steps INTO lw_step.
          IF lv_index >= sy-tabix.
            DELETE lt_steps.
            lv_index = lv_index - 1.
            CONTINUE.
          ENDIF.
        ENDLOOP.

        LOOP AT lt_steps INTO lw_step
          WHERE stepid EQ 'H'
            OR  stepid EQ 'A'
            OR  stepid EQ 'I'
            OR  stepid EQ 'G'
          .
          IF lw_step-rc >= pv_rc.
            pv_rc = lw_step-rc.
          ENDIF.
        ENDLOOP.
        IF sy-subrc NE 0.
          pv_stat = gc_icon_no_logdata.
          PERFORM check_other_trn_group
            USING
              pv_transport
              pv_systemid
              pv_clientid
              pv_category
            CHANGING
              pv_stat
              pv_date
              pv_time
              pv_rc.
        ELSE.
          READ TABLE lt_steps INTO lw_step
            WITH KEY stepid = 'I'.
          lt_actions = lw_step-actions.
          SORT lt_actions
           BY date DESCENDING
            time DESCENDING.
          READ TABLE lt_actions INTO lw_action
           INDEX 1.
          IF sy-subrc NE 0.
            pv_stat = gc_icon_no_logdata.
          ELSE.
            pv_date = lw_action-date.
            pv_time = lw_action-time.
            CASE pv_rc.
              WHEN 0.
                pv_stat = gc_icon_imp_success.
              WHEN 4.
                pv_stat = gc_icon_imp_warning.
              WHEN OTHERS.
                pv_stat = gc_icon_imp_error.
            ENDCASE.
          ENDIF.
        ENDIF.
        RETURN.

      ELSEIF pv_category EQ 'CUST'.
        IF pv_clientid IS INITIAL.
          READ TABLE lt_steps INTO lw_step
          WITH KEY stepid = pv_stepid.
        ELSE.
          READ TABLE lt_steps INTO lw_step
          WITH KEY stepid = pv_stepid
                   clientid = pv_clientid.
        ENDIF.
      ENDIF.
    ELSEIF pv_stepid EQ 'E'.
      READ TABLE lt_steps INTO lw_step
        WITH KEY stepid = pv_stepid.
    ENDIF.
    IF sy-subrc NE 0.
      pv_stat = gc_icon_no_logdata.
      PERFORM check_other_trn_group
        USING
          pv_transport
          pv_systemid
          pv_clientid
          pv_category
        CHANGING
          pv_stat
          pv_date
          pv_time
          pv_rc.
    ELSE.
      lt_actions = lw_step-actions.
      SORT lt_actions
       BY date DESCENDING
        time DESCENDING.
      READ TABLE lt_actions INTO lw_action
       INDEX 1.
      IF sy-subrc NE 0.
        pv_stat = gc_icon_no_logdata.
      ELSE.
        pv_rc = lw_action-rc.
        pv_date = lw_action-date.
        pv_time = lw_action-time.
        CASE lw_action-rc.
          WHEN 0.
            pv_stat = gc_icon_imp_success.
          WHEN 4.
            pv_stat = gc_icon_imp_warning.
          WHEN OTHERS.
            pv_stat = gc_icon_imp_error.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.          " IMPORT_STATUS_DETERMINATION
*&---------------------------------------------------------------------*
*&   Form GLOBAL_DATA_REFRESH
*&---------------------------------------------------------------------*
*    Reset all global data
*----------------------------------------------------------------------*
FORM global_data_refresh .
  CLEAR gw_transport.
  REFRESH gt_transport_list.
  CLEAR gw_transport_sub.
  REFRESH gt_transport_sublist.
  CLEAR gw_task.
  REFRESH gt_task_list.
* ALV data
  REFRESH gt_fieldcat.
  CLEAR gw_layout.
ENDFORM.          " GLOBAL_DATA_REFRESH
*&---------------------------------------------------------------------*
*&   Form ADDL_DATA_SELECTION
*&---------------------------------------------------------------------*
*    Read the co_owner in the transport (owner of a task)
*    Read the latest changed date and time (of task)
*----------------------------------------------------------------------*
FORM co_owner_selection .

  DATA:
    lv_date LIKE e070-as4date,
    lv_time LIKE e070-as4time.

  LOOP AT gt_transport_list INTO gw_transport.
    SELECT
     as4user
     as4date
     as4time
     UP TO 1 ROWS
     INTO
      (gw_transport-co_owner,
       lv_date,
       lv_time
      )
     FROM e070
     WHERE strkorr EQ gw_transport-transport
     ORDER BY
      as4date DESCENDING
      as4time DESCENDING
      .
    ENDSELECT.

    IF sy-subrc NE 0.
      gw_transport-co_owner = gw_transport-owner.
    ENDIF.

    SELECT
     as4date
     as4time
     UP TO 1 ROWS
     INTO
      (gw_transport-chg_date,
       gw_transport-chg_time
      )
     FROM e070
     WHERE strkorr EQ gw_transport-transport
      AND as4date GT gw_transport-chg_date
      OR
       (
        as4date EQ gw_transport-chg_date
        AND as4time GT gw_transport-chg_time
       )
     ORDER BY
      as4date DESCENDING
      as4time DESCENDING
      .
    ENDSELECT.

    MODIFY gt_transport_list FROM gw_transport.
  ENDLOOP.

ENDFORM.          " CO_OWNER_SELECTION


*&---------------------------------------------------------------------*
*&   Form GET_CTS_PROJECT
*&---------------------------------------------------------------------*
*    Read the CTS Project that the transport is assigned to
*----------------------------------------------------------------------*
FORM get_cts_project .

  DATA:
    lv_date LIKE e070-as4date,
    lv_time LIKE e070-as4time.

  LOOP AT gt_transport_list INTO gw_transport.
    SELECT SINGLE reference
      INTO gw_transport-project
      FROM e070a
      WHERE trkorr EQ gw_transport-transport
        AND attribute EQ 'SAP_CTS_PROJECT'
        .
    IF sy-subrc EQ 0.
      MODIFY gt_transport_list FROM gw_transport.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "get_cts_project
*&---------------------------------------------------------------------*
*&      Form  CHECK_OTHER_TRN_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STEPS  text
*      -->P_PV_RC  text
*      -->P_PV_DATE  text
*      -->P_PV_TIME  text
*      -->P_ELSE  text
*----------------------------------------------------------------------*
FORM check_other_trn_group
  USING
    pv_transport TYPE e070-trkorr
    pv_systemid TYPE ctslg_system-systemid
    pv_clientid TYPE ctslg_step-clientid
    pv_category TYPE e070-korrdev
  CHANGING
    pv_stat TYPE icon-id
    pv_date TYPE ctslg_action-date
    pv_time TYPE ctslg_action-time
    pv_rc TYPE ctslg_action-rc.

  DATA:
    lw_cofi_lines TYPE tstrfcofil,
    lt_cofi_lines TYPE TABLE OF tstrfcofil,
    lv_sysname    TYPE  tmscsys-sysnam.

  MOVE pv_systemid TO lv_sysname.

  CALL FUNCTION 'TMS_MGR_GET_COFILE'
    EXPORTING
      iv_sysname           = lv_sysname
*     IV_DOMNAME           =
      iv_dirtype           = 'T'
      iv_trkorr            = pv_transport
      iv_read_header       = ' '
*  IMPORTING
*     ES_COFI_HEADER       =
*     EV_PROJECT           =
*     ET_PREDECESSORS      =
*     EV_LOCAL             =
    TABLES
      tt_cofi_lines        = lt_cofi_lines
    EXCEPTIONS
      file_not_found       = 1
      data_transfer_error  = 2
      unknown_system       = 3
      system_not_available = 4
      unknown_service      = 5
      tms_config_error     = 6
      OTHERS               = 7.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.
  LOOP AT lt_cofi_lines INTO lw_cofi_lines
    WHERE tarsystem EQ pv_systemid
      AND function EQ 'I'.
    pv_date = lw_cofi_lines-trdate.
    pv_time = lw_cofi_lines-trtime.
    pv_rc  = lw_cofi_lines-retcode.
    CASE pv_rc.
      WHEN 0.
        pv_stat = gc_icon_imp_success.
      WHEN 4.
        pv_stat = gc_icon_imp_warning.
      WHEN OTHERS.
        pv_stat = gc_icon_imp_error.
    ENDCASE.
  ENDLOOP.

  IF sy-subrc NE 0.
    pv_stat = gc_icon_no_logdata.
  ENDIF.




ENDFORM.                    " CHECK_OTHER_TRN_GROUP
*&---------------------------------------------------------------------*
*&      Form  GET_R3TR_OBJECT
*&---------------------------------------------------------------------*
FORM get_r3tr_object
  TABLES pt_objects_drv STRUCTURE gw_objects
  USING
    p_limu_object
    p_limu_obj_name
    p_deep.

  DATA:
    pw_objects_drv TYPE gty_obj.

  CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
    EXPORTING
      p_limu_objtype = p_limu_object
      p_limu_objname = p_limu_obj_name
    IMPORTING
      p_r3tr_objtype = pw_objects_drv-object
      p_r3tr_objname = pw_objects_drv-obj_name
    EXCEPTIONS
      no_mapping     = 1
      OTHERS         = 2.

  IF sy-subrc EQ 0.
    pw_objects_drv-pgmid = 'R3TR'.
    pw_objects_drv-deep = p_deep.
    APPEND pw_objects_drv TO pt_objects_drv.
  ENDIF.

ENDFORM.                    " GET_R3TR_OBJECT

*&---------------------------------------------------------------------*
*&      Form  GET_LIMU_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R3TR_OBJECT    text
*      -->P_R3TR_OBJ_NAME  text
*----------------------------------------------------------------------*
FORM get_limu_objects
  TABLES
    pt_objects_drv STRUCTURE gw_objects
  USING
    p_pgmid
    p_object
    p_obj_name
    p_deep.

  DATA:
    lw_e071        TYPE e071,
    lt_vrso        TYPE TABLE OF vrso,
    lw_vrso        TYPE vrso,
    pw_objects_drv TYPE gty_obj.

  lw_e071-pgmid = p_pgmid.
  lw_e071-object = p_object.
  lw_e071-obj_name = p_obj_name.

  CALL FUNCTION 'SVRS_RESOLVE_E071_OBJ'
    EXPORTING
      e071_obj            = lw_e071
    TABLES
      obj_tab             = lt_vrso
    EXCEPTIONS
      not_versionable     = 1
      communication_error = 2
      OTHERS              = 3.
  IF sy-subrc EQ 0.
    LOOP AT lt_vrso INTO lw_vrso.
      pw_objects_drv-pgmid = 'LIMU'.
      pw_objects_drv-object = lw_vrso-objtype.
      pw_objects_drv-obj_name = lw_vrso-objname.
      pw_objects_drv-deep = p_deep.
      APPEND pw_objects_drv TO pt_objects_drv.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_R3TR_OBJECT

*** Begin of changes
*** Get dependent objects
FORM get_dependent_objects
  TABLES
    pt_objects_drv STRUCTURE gw_objects.

  DATA: lt_env_tab TYPE TABLE OF senvi,   "Object to check dependencies
        ls_env_tab TYPE senvi.            "Info Environment

  DATA: lv_obj_type TYPE seu_obj,         "Object type
        lv_obj_name TYPE tadir-obj_name,  "Object name
        lv_no_rfc   TYPE abap_bool,       "RFC Flag
        lv_deep     TYPE i,               "Actual Deepness
        lv_tcode    TYPE tcode.           "Transaction Code

  LOOP AT pt_objects_drv ASSIGNING FIELD-SYMBOL(<ls_object>).
*---------- Check if Transaction exist ----------*
    IF <ls_object>-object = 'PROG'.
      CLEAR lv_tcode.
      SELECT tcode FROM tstc UP TO 1 ROWS "#EC CI_SEL_NESTED "#EC CI_GENBUFF
        INTO lv_tcode
       WHERE pgmna = <ls_object>-obj_name.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        lv_deep = <ls_object>-deep + 1.
        PERFORM check_add_object
          TABLES pt_objects_drv
          USING 'R3TR'         "Program ID in Requests and Tasks
                'TRAN'         "Object Type
                lv_tcode       "Object Name in Object Directory
                ls_env_tab     "Info system
                lv_deep.       "Deepness
      ENDIF.
    ENDIF.
*---------- Check deepness ----------*
    lv_deep = <ls_object>-deep + 1. "Set Deepness

*---------- Get object dependecies ----------*
    REFRESH lt_env_tab.
    lv_obj_type = <ls_object>-object.
    lv_obj_name = <ls_object>-obj_name.
    CALL FUNCTION 'REPOSITORY_ENVIRONMENT_RFC'
      EXPORTING
        obj_type        = lv_obj_type
        object_name     = lv_obj_name
      TABLES
        environment_tab = lt_env_tab.
    IF lines( lt_env_tab ) IS INITIAL AND lines( pt_objects_drv ) = 1.

    ELSE.
      DELETE lt_env_tab WHERE type   = lv_obj_type
                          AND object = <ls_object>-obj_name.
      DELETE lt_env_tab WHERE type = 'MESS'.
*---------- Add founded dependecies ----------*
      LOOP AT lt_env_tab INTO ls_env_tab.                "#EC CI_NESTED
        CASE ls_env_tab-type.
          WHEN 'DEVC'.  "c_devc.  "Add from Development class

          WHEN OTHERS.  "Add all others object
            PERFORM check_add_object
              TABLES pt_objects_drv
              USING space              "Program ID in Requests and Tasks
                    ls_env_tab-type    "Object Type
                    ls_env_tab-object  "Object Name in Object Directory
                    ls_env_tab         "Info system
                    lv_deep.           "Deepness
        ENDCASE.
      ENDLOOP.
***      <ls_object>-status = icon_led_green.  "Status checked
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM check_add_object
  TABLES
    pt_objects_drv STRUCTURE gw_objects
  USING
    i_pgmid     TYPE pgmid
    i_object    TYPE any
    i_obj_n     TYPE any
    is_env_tab  TYPE senvi
    i_deep      TYPE i.

  DATA: lo_wb_object TYPE REF TO cl_wb_object. "Repository Object
  DATA: ls_tadir          TYPE tadir,             "Directory of Repository Objects
        ls_wb_object_type TYPE wbobjtype,         "Global WB Type
        ls_object         LIKE LINE OF pt_objects_drv. "Objects to transport line
  DATA: lv_tr_object   TYPE trobjtype,  "Object Type
        lv_tr_obj_name TYPE trobj_name, "Object Name in Object List
        lv_trans_pgmid TYPE pgmid.      "Program ID in Requests and Tasks

  CONSTANTS: lc_r3tr TYPE char4 VALUE 'R3TR'.

*---------- Object convertions ----------*
  IF i_pgmid <> lc_r3tr.
    SELECT pgmid UP TO 1 ROWS FROM tadir                "#EC CI_GENBUFF
      INTO i_pgmid
     WHERE object   = i_object
       AND obj_name = i_obj_n.
    ENDSELECT.

*---------- Is not a TADIR object and Conversion required ----------*
    IF sy-subrc IS NOT INITIAL.
      lv_tr_object   = i_object.
      lv_tr_obj_name = i_obj_n.
      cl_wb_object=>create_from_transport_key( EXPORTING p_object                = lv_tr_object
                                                         p_obj_name              = lv_tr_obj_name
                                               RECEIVING p_wb_object             = lo_wb_object
                                              EXCEPTIONS objecttype_not_existing = 1
                                                         empty_object_key        = 2
                                                         key_not_available       = 3
                                                         OTHERS                  = 4 ).
      IF sy-subrc IS INITIAL.
        lo_wb_object->get_global_wb_key( IMPORTING p_object_type     = ls_wb_object_type
                                        EXCEPTIONS key_not_available = 1
                                                   OTHERS            = 2 ).
        IF sy-subrc IS INITIAL.
          lo_wb_object->get_transport_key( IMPORTING p_pgmid           = lv_trans_pgmid "#EC CI_SUBRC
                                          EXCEPTIONS key_not_available = 1
                                                     OTHERS            = 2 ).
********* Begin of changes
*******---------- TR Object Type ----------*
******            ls_object-object_tr  = lv_tr_object.
********* End of changes

*---------- Check Program ID ----------*
          CASE lv_trans_pgmid.
            WHEN lc_r3tr.  "Main objects
              i_pgmid = lv_trans_pgmid.
            WHEN 'LIMU'.  "Sub object
              CALL FUNCTION 'GET_R3TR_OBJECT_FROM_LIMU_OBJ'
                EXPORTING
                  p_limu_objtype = lv_tr_object
                  p_limu_objname = lv_tr_obj_name
                IMPORTING
                  p_r3tr_objtype = lv_tr_object
                  p_r3tr_objname = lv_tr_obj_name
                EXCEPTIONS
                  no_mapping     = 1
                  OTHERS         = 2.
              IF sy-subrc IS INITIAL.
                ls_object-pgmid    = lc_r3tr.
                ls_object-object   = lv_tr_object.
                ls_object-obj_name = lv_tr_obj_name.
                PERFORM add_object
                  TABLES pt_objects_drv
                  USING     i_deep        "Deepness
                  CHANGING  ls_object.    "Objects table
                RETURN.
              ENDIF.
            WHEN OTHERS.  "Include objects
              i_pgmid = lc_r3tr.
              CALL FUNCTION 'GET_TADIR_TYPE_FROM_WB_TYPE'
                EXPORTING
                  wb_objtype        = ls_wb_object_type-subtype_wb
                IMPORTING
                  transport_objtype = lv_tr_object
                EXCEPTIONS
                  no_mapping_found  = 1
                  no_unique_mapping = 2
                  OTHERS            = 3.
              IF sy-subrc IS INITIAL.
                i_object = lv_tr_object.
                IF is_env_tab-encl_obj IS NOT INITIAL.
                  i_obj_n = is_env_tab-encl_obj.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*---------- Check in TADIR ----------*
  SELECT SINGLE * FROM tadir
    INTO ls_tadir
   WHERE pgmid    = i_pgmid
     AND object   = i_object
     AND obj_name = i_obj_n.
*---------- Add object ----------*
  IF ls_tadir IS NOT INITIAL.
    MOVE-CORRESPONDING ls_tadir TO ls_object.
****---------- Set SAP Generated object status ----------*
***      IF ls_tadir-genflag IS NOT INITIAL.
***        ls_object-status = icon_generate.
***      ENDIF.
*---------- Add object to be checked ----------*
    PERFORM add_object
      TABLES pt_objects_drv
      USING i_deep          "Deepness
      CHANGING ls_object.   "Objects table
*---------- Error Object not valid ----------*
  ELSE.
    IF lines( pt_objects_drv ) > 0. "Skip first object
      ls_object-pgmid    = i_pgmid.
      ls_object-object   = i_object.
      ls_object-obj_name = i_obj_n.
***        ls_object-status   = icon_led_red.
      PERFORM add_object
        TABLES pt_objects_drv
        USING i_deep          "Deepness
        CHANGING ls_object.   "Objects table
    ENDIF.
  ENDIF.
ENDFORM.

FORM add_object
  TABLES
    pt_objects_drv STRUCTURE gw_objects
  USING
    i_deep    TYPE i            "Deepness
  CHANGING
    cs_object LIKE gw_objects.  "Objects table

  DATA:
    lt_devclass TYPE scts_devclass,       "Development Packages
    ls_devclass TYPE trdevclass.
  DATA: lv_object    TYPE trobjtype,  "Object Type
        lv_objname   TYPE sobj_name,  "Object Name in Object Directory
        lv_namespace TYPE namespace.  "Object Namespace
*---------- Check if already added ----------*
  READ TABLE pt_objects_drv TRANSPORTING NO FIELDS WITH KEY pgmid      = cs_object-pgmid
                                                            object     = cs_object-object
                                                            obj_name   = cs_object-obj_name.
  IF sy-subrc IS NOT INITIAL. "New object
*---------- Check if is customer objects ----------*
    lv_object  = cs_object-object.
    lv_objname = cs_object-obj_name.
    CALL FUNCTION 'TRINT_GET_NAMESPACE'      "#EC FB_RC "#EC CI_SUBRC
      EXPORTING
        iv_pgmid            = cs_object-pgmid
        iv_object           = lv_object
        iv_obj_name         = lv_objname
      IMPORTING
        ev_namespace        = lv_namespace
      EXCEPTIONS
        invalid_prefix      = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF lv_namespace = '/0CUST/'.  " OR v_st IS NOT INITIAL.  "Is customer object
*---------- Add object deepness ----------*
      IF cs_object-deep IS INITIAL.
        cs_object-deep = i_deep.
      ENDIF.
*---------- Add object to transport ----------*
      APPEND cs_object TO pt_objects_drv.
    ENDIF.
  ENDIF.
ENDFORM.
*** End of changes

*** Begin of change - TR Tool V2
FORM check_object_typ_comp.

  DATA(lt_object_ddic) = VALUE typ_r_objecttype(
                              ( sign = 'I' option = 'EQ' low = 'DCLS' )
                              ( sign = 'I' option = 'EQ' low = 'DDLS' )
                              ( sign = 'I' option = 'EQ' low = 'DDLX' )
                              ( sign = 'I' option = 'EQ' low = 'DOMA' )
                              ( sign = 'I' option = 'EQ' low = 'DTEL' )
                              ( sign = 'I' option = 'EQ' low = 'SHLP' )
                              ( sign = 'I' option = 'EQ' low = 'STRU' )
                              ( sign = 'I' option = 'EQ' low = 'TABL' )
                              ( sign = 'I' option = 'EQ' low = 'TABT' )
                              ( sign = 'I' option = 'EQ' low = 'TTYP' )
                              ( sign = 'I' option = 'EQ' low = 'VIEW' ) ).

  IF gt_transport_list IS NOT INITIAL.
    SELECT
     trkorr,
     as4pos,
     pgmid,
     object,
     obj_name
     INTO TABLE @DATA(lt_objects)
     FROM e071
     FOR ALL ENTRIES IN @gt_transport_list
     WHERE trkorr EQ @gt_transport_list-transport
       AND pgmid IN ('LANG','LIMU','R3TR')
       AND object NOT IN ('VDAT','CDAT','TABU','TDAT').
    IF sy-subrc EQ 0.
      SORT lt_objects BY trkorr as4pos.
    ENDIF.
  ENDIF.

  LOOP AT lt_objects ASSIGNING FIELD-SYMBOL(<fs_objects>).
    IF <fs_objects>-object = 'TABL'.
      IF <fs_objects>-obj_name+0(1) NA 'YZ'.
        TRY .
            ASSIGN gt_transport_list[ transport = <fs_objects>-trkorr ] TO FIELD-SYMBOL(<fs_transport>).
            IF <fs_transport> IS ASSIGNED.
              <fs_transport>-comp_typ = 'Standard Table changes'.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.
    ENDIF.

    IF <fs_objects>-object IN lt_object_ddic.
      DATA(lv_ddic) = abap_true.
    ELSE.
      DATA(lv_prog) = abap_true.
    ENDIF.

    AT END OF trkorr.
      TRY .
          ASSIGN gt_transport_list[ transport = <fs_objects>-trkorr ] TO <fs_transport>.
          IF <fs_transport> IS ASSIGNED.
            IF lv_ddic IS NOT INITIAL AND lv_prog IS NOT INITIAL.
              <fs_transport>-obj_typ = 'DDIC/PROG Objects'.
            ELSEIF lv_ddic IS NOT INITIAL AND lv_prog IS INITIAL.
              <fs_transport>-obj_typ = 'DDIC Objects'.
            ELSEIF lv_ddic IS INITIAL AND lv_prog IS NOT INITIAL.
              <fs_transport>-obj_typ = 'PROG Objects'.
            ENDIF.
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      CLEAR: lv_ddic, lv_prog.
    ENDAT.
  ENDLOOP.

ENDFORM.
*** End of change - TR Tool V2
