PROGRAM zmacros.
* Author: Sikidin A.P.


PARAMETERS
: p_loop TYPE string
.

TYPES:
  BEGIN OF t_zmacros_h                  ,
    clnt    TYPE  mandt,
    bname   TYPE  xubname,
    ddtext  TYPE  as4text,
    version TYPE  tdversion,
    id      TYPE  f_id,
  END OF t_zmacros_h                    ,

  BEGIN OF t_zmacros_v                  ,
    clnt      TYPE  mandt,
    id        TYPE  f_id,
    tabname   TYPE  f_id,
    rownumber TYPE  f_id,
    field     TYPE  f_id,
    value     TYPE  fieldname,
  END OF t_zmacros_v                    ,

  BEGIN OF t_zmacros_ddtext             ,
    ddtext TYPE  as4text,
  END OF t_zmacros_ddtext               ,

  BEGIN OF t_zmacros_s                  ,
    clnt TYPE  mandt,
    id   TYPE  f_id,
    line TYPE  int4,
    text TYPE  tdline,

  END OF   t_zmacros_s                  .



TYPES


  : BEGIN OF t_pair
  ,  key TYPE string
  ,  value TYPE string
  , END OF t_pair,


BEGIN OF t_xz_table,
tabname   TYPE tabname,
container TYPE REF TO cl_gui_container,
alv       TYPE REF TO cl_gui_alv_grid,
fieldcat  TYPE lvc_t_fcat,
layout    TYPE lvc_s_layo,
exclude   TYPE ui_functions,
lt_data   TYPE REF TO data,
END OF t_xz_table.

CLASS lcl_tables DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:
      load IMPORTING iv_id             TYPE f_id,
      save IMPORTING iv_id             TYPE f_id
      .

    DATA
          : section_count TYPE i
          , lt_table  TYPE TABLE OF t_xz_table
          .

ENDCLASS.

CLASS lcl_tables IMPLEMENTATION.
  METHOD load.

    DATA
          : lt_xz TYPE TABLE OF t_zmacros_v
          , lv_tabname TYPE tabname
          .

    FIELD-SYMBOLS
                   : <fs_xz> TYPE t_zmacros_v
                   , <fs_table> TYPE t_xz_table
                   , <fs_line> TYPE any
                   , <fs_alv_table> TYPE STANDARD  TABLE
                   , <fs_field> TYPE any
                   .

    SELECT (' * ')
              INTO CORRESPONDING FIELDS OF TABLE lt_xz
              FROM ('ZMACROS_V')
              WHERE id = iv_id
              ORDER BY ('TABNAME ROWNUMBER FIELD').

    CHECK sy-subrc = 0.

    LOOP AT lt_xz ASSIGNING <fs_xz>.
      AT NEW tabname.
        UNASSIGN <fs_alv_table>.
        lv_tabname  = <fs_xz>-tabname.
        CONDENSE lv_tabname.

        IF lv_tabname = '0'.
          lv_tabname = 'MAIN'.
        ENDIF.

        READ TABLE lt_table ASSIGNING <fs_table> WITH KEY tabname = lv_tabname.
        IF sy-subrc = 0.
          ASSIGN <fs_table>-lt_data->* TO <fs_alv_table>.
        ENDIF.
      ENDAT.

      CHECK  <fs_alv_table> IS ASSIGNED .

      READ TABLE <fs_alv_table> ASSIGNING <fs_line> INDEX <fs_xz>-rownumber.

      IF sy-subrc NE 0.
        APPEND INITIAL LINE TO <fs_alv_table> ASSIGNING <fs_line>.
      ENDIF.

      ASSIGN COMPONENT <fs_xz>-field OF STRUCTURE <fs_line> TO <fs_field>.

      IF sy-subrc = 0.
        <fs_field> = <fs_xz>-value.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD save.

    DATA
          : lt_xz TYPE TABLE OF t_zmacros_v
           ,lv_tabix TYPE i
           , lv_index TYPE i
          .


    FIELD-SYMBOLS
                   : <fs_table> TYPE t_xz_table
                   , <fs_xz> TYPE t_zmacros_v
                   , <fs_alv_table> TYPE STANDARD  TABLE
                   , <fs_line> TYPE any
                   , <fs_field> TYPE any
                   .

    LOOP AT lt_table ASSIGNING <fs_table> .

      ASSIGN <fs_table>-lt_data->* TO <fs_alv_table>.

      LOOP AT <fs_alv_table> ASSIGNING  <fs_line>.
        lv_tabix = sy-tabix.

        DO .
          ASSIGN COMPONENT sy-index OF STRUCTURE <fs_line> TO <fs_field>.

          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
          lv_index = sy-index.

          APPEND INITIAL LINE TO lt_xz ASSIGNING <fs_xz>.

          <fs_xz>-id = iv_id.
          IF <fs_table>-tabname = 'MAIN'.
            <fs_xz>-tabname = 0.
          ELSE.
            <fs_xz>-tabname = <fs_table>-tabname.
          ENDIF.

          <fs_xz>-rownumber = lv_tabix.
          <fs_xz>-field = lv_index .
          <fs_xz>-value = <fs_field>.

        ENDDO.

      ENDLOOP.


    ENDLOOP.

    INSERT ('ZMACROS_V') FROM  TABLE lt_xz.


  ENDMETHOD.

ENDCLASS.

CLASS display DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES
    :  t_itext TYPE STANDARD TABLE OF tline-tdline
    .


    CLASS-DATA:
      read_only TYPE abap_bool READ-ONLY VALUE abap_false.

    CLASS-METHODS:

      fill_abap_editor   IMPORTING editor TYPE REF TO cl_gui_abapedit
                                   text   TYPE  t_itext,
      read_abap_editor   IMPORTING editor TYPE REF TO cl_gui_abapedit
                         EXPORTING text   TYPE t_itext,
      create_abap_editor
        IMPORTING parent_container TYPE REF TO cl_gui_container
        RETURNING VALUE(editor)    TYPE REF TO cl_gui_abapedit.



ENDCLASS.

CLASS cls_event_handler DEFINITION DEFERRED.

CLASS cls_event_handler DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
*     Handles method function_selected  for the toolbar control
      on_function_selected FOR EVENT function_selected
                    OF cl_gui_toolbar
        IMPORTING fcode,

      on_context_menu FOR EVENT   context_menu OF cl_gui_abapedit
        IMPORTING menu ,
      on_context_menu_selected FOR EVENT context_menu_selected OF cl_gui_abapedit
        IMPORTING fcode
        .

ENDCLASS.                    "cls_event_handler DEFINITION



DATA
      : splitter TYPE REF TO cl_gui_splitter_container
      , splitter2 TYPE REF TO cl_gui_splitter_container
      , split_text TYPE REF TO cl_gui_splitter_container
      , docking_left TYPE REF TO cl_gui_docking_container
      , dock_sub_cont1 TYPE REF TO cl_gui_container
      , dock_sub_cont2 TYPE REF TO cl_gui_container

      , dock_sub_toolbar TYPE REF TO cl_gui_container
      , dock_sub_src TYPE REF TO cl_gui_container
      , dock_sub_dst TYPE REF TO cl_gui_container

      , g_text_editor_src    TYPE REF TO cl_gui_abapedit
      , g_text_editor_dst    TYPE REF TO cl_gui_abapedit

      , go_toolbar              TYPE REF TO cl_gui_toolbar
      , gt_button_group            TYPE ttb_button
      , gt_events                  TYPE cntl_simple_events
      , gs_event                   TYPE cntl_simple_event

      , go_event_handler        TYPE REF TO cls_event_handler

  , gv_key_1 TYPE string
  , gv_value_1 TYPE string
  , gv_value_2 TYPE string
  , gv_value_3 TYPE string

      , lv_item_count TYPE i
      , lv_from  TYPE string
      , lv_from_1  TYPE string
      , lv_to TYPE string
      , lv_from_i TYPE i
      , lv_value TYPE i
      , lv_to_i TYPE i

      , lv_length TYPE i

      , lv_n1(1) TYPE n
      , lv_n2(2) TYPE n
      , lv_n3(3) TYPE n
      , lv_n4(4) TYPE n
      , lv_n5(5) TYPE n
      , lv_n6(6) TYPE n
      , lv_n7(7) TYPE n
      , lv_n8(8) TYPE n
      , lv_n9(9) TYPE n
      , lv_n10(10) TYPE n
      , lv_i TYPE i



      , itext_t TYPE TABLE OF tline-tdline
      , itext_dest TYPE TABLE OF tline-tdline
      , itext_buf TYPE TABLE OF tline-tdline

      , lr_table TYPE REF TO lcl_tables
      .

FIELD-SYMBOLS
  : <fs_line> TYPE tdline
  , <fs_alv_table> TYPE STANDARD  TABLE
  , <fs_alv_line> TYPE any
  , <fs_field> TYPE any
  , <fs_transform> TYPE any
  .


INITIALIZATION.
  p_loop = '22'.

AT SELECTION-SCREEN OUTPUT.

  IF docking_left IS INITIAL.


    CREATE OBJECT:

    lr_table,

    docking_left
    EXPORTING repid = sy-repid
    dynnr = sy-dynnr
    side = docking_left->dock_at_top
    extension = 1000,

    splitter
    EXPORTING parent = docking_left
    rows = 3
    columns = 1


    .

    CALL METHOD:

       splitter->set_border
       EXPORTING border = space,


       splitter->get_container
       EXPORTING row = 1
       column = 1
       RECEIVING container = dock_sub_toolbar,

       splitter->get_container
       EXPORTING row = 2
       column = 1
       RECEIVING container = dock_sub_cont1,

       splitter->get_container
       EXPORTING row = 3
       column = 1
       RECEIVING container = dock_sub_cont2,



           splitter->set_row_height
           EXPORTING id = 1
             height = '2',

           splitter->set_row_height
           EXPORTING id = 2
             height = '15',

       splitter->set_row_height
       EXPORTING id = 3
       height = '30',

       splitter->set_row_height
       EXPORTING id = 4
       height = '30'
       .


*/--------------toolbar_here----------------------
    CREATE OBJECT go_toolbar
      EXPORTING
        parent = dock_sub_toolbar.
* Add buttons to the toolbar
    PERFORM add_button_group.

* Create event table. The event ID must be found in the
* documentation of the specific control
    CLEAR gs_event.
    REFRESH gt_events.
    gs_event-eventid    = go_toolbar->m_id_function_selected.
    gs_event-appl_event = 'X'.           " This is an application event
    APPEND gs_event TO gt_events.

* Use the events table to register events for the control
    CALL METHOD go_toolbar->set_registered_events
      EXPORTING
        events = gt_events.

* Create event handlers
    CREATE OBJECT go_event_handler.

    SET HANDLER go_event_handler->on_function_selected
            FOR go_toolbar.

    "-------------event handler-----------------

    CREATE OBJECT split_text
      EXPORTING
        parent  = dock_sub_cont2
        rows    = 1
        columns = 2.

    CALL METHOD:

       split_text->set_border
       EXPORTING border = space,


       split_text->get_container
       EXPORTING row = 1
       column = 1
       RECEIVING container = dock_sub_src,

       split_text->get_container
       EXPORTING row = 1
       column = 2
       RECEIVING container = dock_sub_dst.

    g_text_editor_src  = display=>create_abap_editor( dock_sub_src ).

    CALL METHOD g_text_editor_src->register_event_context_menu
      EXPORTING
        register                 = 1
        appl_event               = 'X'
        local_entries            = 0
      EXCEPTIONS
        error_regist_event       = 1
        error_unregist_event     = 2
        cntl_error               = 3
        event_already_registered = 4
        event_not_registered     = 5
        OTHERS                   = 6.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    display=>fill_abap_editor( editor = g_text_editor_src
                           text   = itext_t[] ).



    SET HANDLER go_event_handler->on_context_menu FOR g_text_editor_src.
    SET HANDLER go_event_handler->on_context_menu_selected FOR g_text_editor_src.


    g_text_editor_dst  = display=>create_abap_editor( dock_sub_dst ).

  ELSE.
    LOOP AT lr_table->lt_table ASSIGNING  FIELD-SYMBOL(<fs_table>) .
      CHECK <fs_table>-lt_data IS NOT INITIAL.

      ASSIGN <fs_table>-lt_data->* TO <fs_alv_table>.

      DESCRIBE TABLE  <fs_alv_table>  LINES DATA(lv_lines).

      IF lv_lines = 1.
        READ TABLE <fs_alv_table> ASSIGNING <fs_alv_line> INDEX 1.
        lv_item_count = 0.

        DO .

          ASSIGN COMPONENT sy-index OF STRUCTURE <fs_alv_line> TO <fs_field>.
          IF sy-subrc = 0.
            ADD 1 TO lv_item_count.
          ELSE.
            EXIT.

          ENDIF.

        ENDDO.


        IF lv_item_count = 1.
          FIND REGEX '[\$_](\d+)-(\d+)' IN <fs_field>  .

          IF sy-subrc = 0 .
            SPLIT <fs_field> AT '-' INTO lv_from lv_to .
            lv_from_1 = lv_from(1).
            SHIFT lv_from LEFT DELETING LEADING '_'.
            SHIFT lv_from LEFT DELETING LEADING '$'.

            IF lv_from(1) = '0'.
              lv_length = strlen( lv_to ).

              CASE lv_length.
                WHEN 1.
                  ASSIGN lv_n1 TO <fs_transform>.
                WHEN 2.
                  ASSIGN lv_n2 TO <fs_transform>.
                WHEN 3.
                  ASSIGN lv_n3 TO <fs_transform>.
                WHEN 4.
                  ASSIGN lv_n4 TO <fs_transform>.
                WHEN 5.
                  ASSIGN lv_n5 TO <fs_transform>.
                WHEN 6.
                  ASSIGN lv_n6 TO <fs_transform>.
                WHEN 7.
                  ASSIGN lv_n7 TO <fs_transform>.
                WHEN 8.
                  ASSIGN lv_n8 TO <fs_transform>.
                WHEN 9.
                  ASSIGN lv_n9 TO <fs_transform>.
                WHEN 10.
                  ASSIGN lv_n10 TO <fs_transform>.
              ENDCASE.

            ELSE.
              ASSIGN lv_i TO <fs_transform>.

            ENDIF.

            lv_from_i = lv_from.
            lv_to_i = lv_to.

            REFRESH <fs_alv_table>.

            DO  lv_to_i TIMES.
              CASE lv_from_1.
                WHEN '_'.
                  lv_value = lv_from_i + sy-index - 1.
                WHEN '$'.
                  IF sy-index < lv_from_i.
                    CONTINUE.
                  ENDIF.
                  lv_value =     sy-index.
              ENDCASE.


              <fs_transform> = lv_value.
              APPEND INITIAL LINE TO <fs_alv_table> ASSIGNING <fs_alv_line>.
              <fs_alv_line> = <fs_transform>.


            ENDDO.


          ENDIF.

        ENDIF.


      ENDIF.

      PERFORM recalc_cwidth_opt USING <fs_table>-alv.

      CALL METHOD <fs_table>-alv->refresh_table_display
        EXPORTING
          i_soft_refresh = 'X'.

    ENDLOOP.



  ENDIF.


CLASS display IMPLEMENTATION.

  METHOD create_abap_editor.
    CREATE OBJECT editor
      EXPORTING
        parent = parent_container.
    editor->set_toolbar_mode( 0 ).
    editor->set_statusbar_mode( 0 ).
    IF read_only = abap_true.
      editor->set_readonly_mode( 1 ).
    ELSE.
      editor->set_readonly_mode( 0 ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_abap_editor.
    editor->set_text( text ).
  ENDMETHOD.

  METHOD read_abap_editor.
    editor->get_text( IMPORTING table = text ).
  ENDMETHOD.

ENDCLASS.


FORM refresh.

  FIELD-SYMBOLS
    : <it_outtab> TYPE STANDARD TABLE
    .


  DATA
        : itext_t TYPE TABLE OF tline-tdline
        , result_tab TYPE match_result_tab
        , lt_tab_dimension TYPE TABLE OF t_pair
        .

  display=>read_abap_editor( EXPORTING editor = g_text_editor_src
                             IMPORTING text   = itext_t[] ).

  LOOP AT lr_table->lt_table ASSIGNING FIELD-SYMBOL(<fs_table>).
    FREE
    : <fs_table>-lt_data
    ,  <fs_table>-alv
    , <fs_table>-container

    .

  ENDLOOP.

  REFRESH lr_table->lt_table.

  DATA
        : lv_temp_token  TYPE string
        , lv_token TYPE string
        , lt_token_stack TYPE TABLE OF string
        .

  LOOP AT itext_t ASSIGNING <fs_line>.
    FIND ALL OCCURRENCES OF REGEX '[<>]\d+' IN <fs_line>  RESULTS result_tab.
    LOOP AT result_tab ASSIGNING FIELD-SYMBOL(<fs_result>).
      lv_token = <fs_line>+<fs_result>-offset(<fs_result>-length).

      CASE lv_token(1).
        WHEN '>'.
          READ TABLE lt_token_stack TRANSPORTING NO FIELDS WITH KEY table_line = lv_token.
          IF sy-subrc = 0.
            MESSAGE e000(38) WITH 'Double nesting token' lv_token .
          ELSE.
            INSERT lv_token INTO lt_token_stack   INDEX 1 .
          ENDIF.

        WHEN '<'.
          READ TABLE lt_token_stack INDEX 1 INTO lv_temp_token.
          IF sy-subrc NE 0.
            MESSAGE e000(38) WITH 'Incorect closing' lv_token .
          ENDIF.

          TRANSLATE lv_temp_token USING '><'.

          IF lv_temp_token NE lv_token .
            MESSAGE e000(38) WITH 'Incorect closing' lv_temp_token  lv_token .
          ENDIF.

          DELETE lt_token_stack INDEX 1.
      ENDCASE.

    ENDLOOP.
  ENDLOOP.

  IF lt_token_stack IS NOT INITIAL .
    READ TABLE lt_token_stack INDEX 1 INTO lv_temp_token.
    MESSAGE e000(38) WITH 'No closing for' lv_temp_token  lv_token .
  ENDIF.

  LOOP AT itext_t ASSIGNING <fs_line>.

    FIND ALL OCCURRENCES OF REGEX '\$(\d+)-(\d+)#' IN <fs_line>  RESULTS result_tab.

    LOOP AT result_tab ASSIGNING <fs_result>.

      APPEND INITIAL LINE TO lt_tab_dimension ASSIGNING FIELD-SYMBOL(<fs_dimension>).

      READ TABLE  <fs_result>-submatches ASSIGNING FIELD-SYMBOL(<fs_submatch>) INDEX 1.
      <fs_dimension>-key = <fs_line>+<fs_submatch>-offset(<fs_submatch>-length).

      READ TABLE  <fs_result>-submatches ASSIGNING <fs_submatch> INDEX 2.
      <fs_dimension>-value = <fs_line>+<fs_submatch>-offset(<fs_submatch>-length).


    ENDLOOP.

  ENDLOOP.


  SORT lt_tab_dimension BY key value DESCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_tab_dimension COMPARING key.

  lr_table->section_count = lines( lt_tab_dimension ) + 1.


  IF splitter2 IS BOUND.
    CALL METHOD splitter2->set_grid
      EXPORTING
        rows              = 1
        columns           = lr_table->section_count
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ELSE.
    CREATE OBJECT splitter2
      EXPORTING
        parent  = dock_sub_cont1
        rows    = 1
        columns = lr_table->section_count.
  ENDIF.

  CALL METHOD splitter2->set_border
    EXPORTING
      border = space.



  APPEND INITIAL LINE TO  lr_table->lt_table ASSIGNING <fs_table> .

  <fs_table>-tabname = 'MAIN'.


  CALL METHOD splitter2->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = <fs_table>-container.

  CREATE OBJECT <fs_table>-alv
    EXPORTING
      i_appl_events = 'X'
      i_parent      = <fs_table>-container.

  "---------event-edit-register
  CALL METHOD <fs_table>-alv->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  PERFORM prepare_field_catalog_tabname CHANGING <fs_table>-fieldcat .
  PERFORM prepare_layout_tabname USING 'MAIN' CHANGING <fs_table>-layout .
  PERFORM exclude_tb_functions_tabname CHANGING <fs_table>-exclude.

  DATA
        : ifc TYPE lvc_t_fcat
        .

  APPEND INITIAL LINE TO ifc ASSIGNING FIELD-SYMBOL(<fs_ifc>).

  <fs_ifc>-fieldname = 'TABNAME' .
  <fs_ifc>-lowercase = 'X' .
  <fs_ifc>-datatype = 'C'.
  <fs_ifc>-inttype = 'C'.
  <fs_ifc>-intlen = 30.

  APPEND INITIAL LINE TO ifc ASSIGNING <fs_ifc>.

  <fs_ifc>-fieldname = 'ALIAS' .
  <fs_ifc>-lowercase = 'X' .
  <fs_ifc>-datatype = 'C'.
  <fs_ifc>-inttype = 'C'.
  <fs_ifc>-intlen = 30.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = ifc
    IMPORTING
      ep_table                  = <fs_table>-lt_data
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Generation Limit Reached' TYPE 'I'.
    RETURN.
  ENDIF.

  ASSIGN <fs_table>-lt_data->* TO <it_outtab>.

  LOOP AT itext_t ASSIGNING <fs_line>.
    FIND ALL OCCURRENCES OF REGEX '\\[^/.]*/' IN <fs_line> RESULTS result_tab.
    LOOP AT result_tab ASSIGNING <fs_result>.
      lv_token = <fs_line>+<fs_result>-offset(<fs_result>-length).
      APPEND INITIAL LINE TO <it_outtab> ASSIGNING FIELD-SYMBOL(<fs_any>).
      ASSIGN COMPONENT 1 OF STRUCTURE <fs_any> TO FIELD-SYMBOL(<fs_field>).
      <fs_field>  = lv_token.
    ENDLOOP.

  ENDLOOP.

  SORT <it_outtab>.
  DELETE ADJACENT DUPLICATES FROM <it_outtab>.


  CALL METHOD <fs_table>-alv->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = <fs_table>-exclude
      is_layout            = <fs_table>-layout
    CHANGING
      it_outtab            = <it_outtab>
      it_fieldcatalog      = <fs_table>-fieldcat.



  LOOP AT lt_tab_dimension ASSIGNING <fs_dimension>.

    PERFORM register_table
                USING
                   lr_table
                   splitter2
                   sy-tabix
                   <fs_dimension>.


  ENDLOOP.


ENDFORM.



FORM do_stuff.

  display=>read_abap_editor( EXPORTING editor = g_text_editor_src
                             IMPORTING text   = itext_t[] ).

  LOOP AT lr_table->lt_table ASSIGNING FIELD-SYMBOL(<fs_xz_refresh>).
    <fs_xz_refresh>-alv->check_changed_data( ).
  ENDLOOP.

  itext_buf = itext_t.

  FIELD-SYMBOLS
                 : <fs_main> TYPE t_xz_table
                 , <fs_table> TYPE STANDARD TABLE
                 .

  READ TABLE lr_table->lt_table ASSIGNING <fs_main> WITH KEY tabname = 'MAIN'.

  ASSIGN <fs_main>-lt_data->* TO <fs_table>.

  LOOP AT itext_buf ASSIGNING FIELD-SYMBOL(<fs_buf>).

    LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line>).
      ASSIGN COMPONENT 1 OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_key>).
      ASSIGN COMPONENT 2 OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_value>).

      REPLACE ALL OCCURRENCES OF <fs_key> IN <fs_buf> WITH <fs_value>.

    ENDLOOP.

  ENDLOOP.

  REFRESH  itext_dest.

  PERFORM recursive_replace
              USING
                 ''
                 ''
                 itext_buf
              CHANGING
                 itext_dest.

  display=>fill_abap_editor( editor = g_text_editor_dst
                             text   = itext_dest[] ).

ENDFORM.

FORM prepare_field_catalog_tabname CHANGING pt_fieldcat TYPE lvc_t_fcat .
  DATA ls_fcat TYPE lvc_s_fcat .
  REFRESH: pt_fieldcat.
  ls_fcat-fieldname = 'TABNAME' .
  ls_fcat-inttype   = 'C' .
  ls_fcat-lowercase   = 'X' .
  ls_fcat-outputlen = '30' .
  ls_fcat-coltext   = '________KEY________' .
  ls_fcat-seltext   = 'KEY' .
*  ls_fcat-edit      = 'X'.
  APPEND ls_fcat TO pt_fieldcat .

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ALIAS' .
  ls_fcat-lowercase = 'X' .
  ls_fcat-inttype   = 'C' .
  ls_fcat-outputlen = '30' .
  ls_fcat-coltext   = '___Value___' .
  ls_fcat-seltext   = '___Value___' .
  ls_fcat-edit      = 'X'.
  APPEND ls_fcat TO pt_fieldcat .
ENDFORM. "prepare_field_catalog_tabname

FORM prepare_field_catalog_uni USING lv_i TYPE i CHANGING pt_fieldcat TYPE lvc_t_fcat .

  DO lv_i TIMES.
    APPEND INITIAL LINE TO pt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-fieldname = |FIELD{ sy-index }| .
    <fs_fcat>-inttype   = 'C' .
    <fs_fcat>-outputlen = '120' .
    <fs_fcat>-lowercase = 'X' .
    <fs_fcat>-coltext   = |FIELD{ sy-index }| .
    <fs_fcat>-seltext   = |FIELD{ sy-index }| .
    <fs_fcat>-edit      = 'X'.

  ENDDO.


ENDFORM. "prepare_field_catalog_tabname

FORM prepare_layout_tabname USING title TYPE lvc_title CHANGING p_gs_layout_tabname TYPE lvc_s_layo.
  p_gs_layout_tabname-cwidth_opt = 'X'.
  p_gs_layout_tabname-grid_title = title.
  p_gs_layout_tabname-sel_mode   = 'A'.
  p_gs_layout_tabname-zebra      = 'X'.
ENDFORM. " PREPARE_LAYOUT_TABNAME

FORM exclude_tb_functions_tabname CHANGING pt_exclude TYPE ui_functions.
  APPEND '&CHECK' TO pt_exclude.
  APPEND '&REFRESH' TO pt_exclude.
  APPEND '&LOCAL&CUT' TO pt_exclude.
  APPEND '&LOCAL#' TO pt_exclude.
  APPEND '&LOCAL&PASTE' TO pt_exclude.
  APPEND '&LOCAL&UNDO' TO pt_exclude.
  APPEND '&DETAIL' TO pt_exclude.
  APPEND '&LOCAL&COPY_ROW' TO pt_exclude.
  APPEND '&LOCAL&INSERT_ROW' TO pt_exclude.
  APPEND '&SORT_ASC' TO pt_exclude.
  APPEND '&SORT_DSC' TO pt_exclude.
  APPEND '&FIND' TO pt_exclude.
  APPEND '&FIND_MORE' TO pt_exclude.
  APPEND '&MB_FILTER' TO pt_exclude.
  APPEND '&MB_SUM' TO pt_exclude.
  APPEND '&MB_SUBTOT' TO pt_exclude.
  APPEND '&PRINT_BACK' TO pt_exclude.
  APPEND '&MB_VIEW' TO pt_exclude.
  APPEND '&MB_EXPORT' TO pt_exclude.
  APPEND '&MB_VARIANT' TO pt_exclude.
  APPEND '&GRAPH' TO pt_exclude.
  APPEND '&INFO' TO pt_exclude.
ENDFORM. " EXCLUDE_TB_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form  REGISTER_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LR_TABLE  text
*      -->P_SPLITTER2  text
*      -->P_SY_INDEX  text
*      -->P_<FS_DIMENSION>  text
*----------------------------------------------------------------------*
FORM register_table  USING    lr_table TYPE REF TO lcl_tables
                              splitter TYPE REF TO cl_gui_splitter_container
                              sy_index TYPE syst_index
                              dimension TYPE t_pair .
  DATA
        : lv_ind_plus TYPE syst_index
        .

  lv_ind_plus = sy_index + 1.

  APPEND INITIAL LINE TO  lr_table->lt_table ASSIGNING FIELD-SYMBOL(<fs_table>) .

  <fs_table>-tabname = dimension-key.


  CALL METHOD splitter->get_container
    EXPORTING
      row       = 1
      column    = lv_ind_plus
    RECEIVING
      container = <fs_table>-container.

  CREATE OBJECT <fs_table>-alv
    EXPORTING
      i_appl_events = 'X'
      i_parent      = <fs_table>-container.

  "---------event-edit-register
  CALL METHOD <fs_table>-alv->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  DATA
        : lv_i TYPE i
        .

  lv_i  = dimension-value.


  PERFORM prepare_field_catalog_uni USING lv_i CHANGING <fs_table>-fieldcat .
  DATA
        : lv_title TYPE lvc_title
        .
  lv_title = dimension-key.

  PERFORM prepare_layout_tabname USING lv_title CHANGING <fs_table>-layout .
  PERFORM exclude_tb_functions_tabname CHANGING <fs_table>-exclude.

  DATA
        : ifc TYPE lvc_t_fcat
        .


  DO lv_i TIMES.
    APPEND INITIAL LINE TO ifc ASSIGNING FIELD-SYMBOL(<fs_ifc>).

    <fs_ifc>-fieldname = |FIELD{ sy-index }| .
    <fs_ifc>-datatype = 'C'.
    <fs_ifc>-lowercase = 'X'.
    <fs_ifc>-inttype = 'C'.
    <fs_ifc>-intlen = 120.
  ENDDO.


  FIELD-SYMBOLS
    : <fs_any> TYPE STANDARD TABLE
    .

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = ifc
    IMPORTING
      ep_table                  = <fs_table>-lt_data
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Generation Limit Reached' TYPE 'I'.
    RETURN.
  ENDIF.

  ASSIGN <fs_table>-lt_data->* TO <fs_any>.

  APPEND INITIAL LINE TO <fs_any>.

  CALL METHOD <fs_table>-alv->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = <fs_table>-exclude
      is_layout            = <fs_table>-layout
    CHANGING
      it_outtab            = <fs_any>
      it_fieldcatalog      = <fs_table>-fieldcat.

ENDFORM.


FORM recalc_cwidth_opt USING alv_grid TYPE REF TO cl_gui_alv_grid.

  DATA layout   TYPE lvc_s_layo.

  CALL METHOD alv_grid->get_frontend_layout
    IMPORTING
      es_layout = layout.

  layout-cwidth_opt = 'X'.

  CALL METHOD alv_grid->set_frontend_layout
    EXPORTING
      is_layout = layout.
ENDFORM.                    "recalc_cwidth_opt
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save .
  DATA: wa_zmacros_h      TYPE t_zmacros_h,
        wa_zmacros_s      TYPE t_zmacros_s,

        i_answer          TYPE c,
        i_valueout        TYPE as4text,
        lv_version        TYPE  tdversion,
        lv_id             TYPE  f_id,
        lv_line           LIKE wa_zmacros_s-line,
        wa_zmacros_ddtext TYPE t_zmacros_ddtext,
        ret_tab           LIKE ddshretval OCCURS 0 WITH HEADER LINE,
        it_zmacros_h_t    TYPE TABLE OF t_zmacros_h WITH HEADER LINE,
        it_zmacros_h      TYPE TABLE OF t_zmacros_h WITH HEADER LINE,
        it_zmacros_ddtext TYPE TABLE OF t_zmacros_ddtext WITH HEADER LINE.



  PERFORM detect_table.

  SELECT (' BNAME   DDTEXT MAX( VERSION ) AS VERSION    ID')
        FROM ('ZMACROS_H')
        INTO CORRESPONDING FIELDS OF TABLE it_zmacros_h_t
        GROUP BY ('BNAME   DDTEXT   ID')
        ORDER BY ('ID DESCENDING') .

  LOOP AT it_zmacros_h_t.
    MOVE-CORRESPONDING it_zmacros_h_t TO wa_zmacros_ddtext.
    COLLECT wa_zmacros_ddtext INTO it_zmacros_ddtext.
  ENDLOOP.


  LOOP AT it_zmacros_ddtext.
    CLEAR lv_version .
    LOOP AT it_zmacros_h_t WHERE ddtext = it_zmacros_ddtext-ddtext.
      IF lv_version < it_zmacros_h_t-version.
        lv_version = it_zmacros_h_t-version.
      ENDIF.
    ENDLOOP.

    READ TABLE it_zmacros_h_t WITH KEY ddtext = it_zmacros_ddtext-ddtext version = lv_version.
    APPEND it_zmacros_h_t TO it_zmacros_h.

  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DDTEXT'
      window_title    = 'Select Query'
      value_org       = 'S'
      multiple_choice = ' '
    TABLES
      value_tab       = it_zmacros_h
      return_tab      = ret_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0.
    LOOP AT ret_tab.
      i_valueout  = ret_tab-fieldval .
    ENDLOOP.
  ENDIF.

  IF i_valueout IS INITIAL.
    CALL FUNCTION 'POPUP_TO_GET_VALUE'
      EXPORTING
        fieldname           = 'DDTEXT'
        tabname             = 'DD90T'
        titel               = 'Please enter the query name'
        valuein             = sy-uname
      IMPORTING
        answer              = i_answer
        valueout            = i_valueout
      EXCEPTIONS
        fieldname_not_found = 1
        OTHERS              = 2.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    IF i_answer = 'C'.
      EXIT.
    ENDIF.
  ENDIF.



  SELECT  COUNT( * )  FROM ('ZMACROS_H')
*    into wa_ZMACROS_H
    WHERE ddtext = i_valueout.

  IF sy-subrc = 0.
    SELECT  MAX( version ) INTO lv_version
            FROM ('ZMACROS_H')
            WHERE  ddtext = i_valueout.

    SELECT  MAX( id ) INTO lv_id
            FROM ('ZMACROS_H').

    ADD 1 TO lv_version.
    ADD 1 TO lv_id.
  ELSE.
    lv_version = 1.
    SELECT  MAX( id ) INTO lv_id
            FROM ('ZMACROS_H').
    IF sy-subrc NE 0.
      lv_id = 1.
    ELSE.
      ADD 1 TO lv_id.
    ENDIF.

  ENDIF.

  wa_zmacros_h-bname   = sy-uname.
  wa_zmacros_h-ddtext  = i_valueout.
  wa_zmacros_h-version = lv_version.
  wa_zmacros_h-id      = lv_id.

  INSERT ('ZMACROS_H') FROM wa_zmacros_h.
  lv_line = 0.

  DATA
      : itext_t TYPE TABLE OF tline-tdline
      .

  display=>read_abap_editor( EXPORTING editor = g_text_editor_src
                             IMPORTING text   = itext_t[] ).

  LOOP AT itext_t ASSIGNING FIELD-SYMBOL(<fs_text>).
    CLEAR wa_zmacros_s.
    ADD 1 TO lv_line.

    wa_zmacros_s-id      = lv_id.
    wa_zmacros_s-line    = lv_line.


    wa_zmacros_s-text   = <fs_text>.

    INSERT ('ZMACROS_S') FROM wa_zmacros_s.
  ENDLOOP.


  lr_table->save( lv_id ).

ENDFORM.

FORM range.

  DATA
        : returncode(1)       TYPE c
        .

  TYPES
  : BEGIN OF t_data
  , asd TYPE spo_value
  , END OF t_data
  .

  DATA: BEGIN OF fields OCCURS 1.
      INCLUDE STRUCTURE sval.
  DATA: END OF fields.

  DEFINE m_fields.
    CLEAR fields.
    fields-tabname     = 'SBOOK'.
    fields-fieldname   = &1.
    fields-value       = &2.
    fields-field_obl   = &3.
    fields-fieldtext   = &4.
    fields-field_attr  = &5.
    APPEND fields.
  END-OF-DEFINITION. "m_fields


  m_fields:  'PASSNAME'    ''  ''  'Range'         '00'.


  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title = 'Range'
    IMPORTING
      returncode  = returncode
    TABLES
      fields      = fields.

  CHECK  returncode <> 'A'.

  READ TABLE fields INDEX 1.

  DATA
        : lv_1 TYPE i
        , lv_2 TYPE i
        , lv_sign TYPE i VALUE 1
        , lt_i TYPE TABLE OF t_data
        , lv_c1 TYPE spo_value
        , lv_c2 TYPE spo_value
        , lv_length TYPE i
, lv_n1(1) TYPE n
, lv_n2(2) TYPE n
, lv_n3(3) TYPE n
, lv_n4(4) TYPE n
, lv_n5(5) TYPE n
, lv_n6(6) TYPE n
, lv_n7(7) TYPE n
, lv_n8(8) TYPE n
, lv_n9(9) TYPE n
, lv_n10(10) TYPE n
, lv_xz TYPE spo_value

, lv_i TYPE i

        .

  FIELD-SYMBOLS
                 : <fs_data> TYPE t_data
                 , <fs_transform> TYPE any
                 .

  SPLIT fields-value AT '-' INTO lv_c1 lv_c2.

  lv_1 = lv_c1.
  lv_2 = lv_c2.

  IF lv_1 > lv_2.
    lv_sign = -1.
    lv_length = strlen( lv_c1 ).
    lv_xz = lv_1.
  ELSE.
    lv_length = strlen( lv_c2 ).
    lv_xz  = lv_2.
  ENDIF.


  IF lv_c1(1) = '0'.


    CASE lv_length.
      WHEN 1.
        ASSIGN lv_n1 TO <fs_transform>.
      WHEN 2.
        ASSIGN lv_n2 TO <fs_transform>.
      WHEN 3.
        ASSIGN lv_n3 TO <fs_transform>.
      WHEN 4.
        ASSIGN lv_n4 TO <fs_transform>.
      WHEN 5.
        ASSIGN lv_n5 TO <fs_transform>.
      WHEN 6.
        ASSIGN lv_n6 TO <fs_transform>.
      WHEN 7.
        ASSIGN lv_n7 TO <fs_transform>.
      WHEN 8.
        ASSIGN lv_n8 TO <fs_transform>.
      WHEN 9.
        ASSIGN lv_n9 TO <fs_transform>.
      WHEN 10.
        ASSIGN lv_n10 TO <fs_transform>.
    ENDCASE.

  ELSE.
    ASSIGN lv_i TO <fs_transform>.

  ENDIF.


  DO .

    APPEND INITIAL LINE TO lt_i ASSIGNING <fs_data>.

    <fs_transform> = lv_1.

    <fs_data>-asd = <fs_transform>.
    CONDENSE <fs_data>-asd.

    IF lv_1 = lv_2.
      EXIT.
    ENDIF.

    ADD lv_sign TO lv_1.


  ENDDO.
  DATA
        : lv_rc TYPE i
        .


  CALL METHOD cl_gui_frontend_services=>clipboard_export
*  EXPORTING
*    no_auth_check        = SPACE
    IMPORTING
      data                 = lt_i
    CHANGING
      rc                   = lv_rc
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



  MESSAGE 'Exported' TYPE 'S'.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list  USING p_all TYPE c.


  DATA:

    wa_zmacros_ddtext TYPE t_zmacros_ddtext,
    it_zmacros_ddtext TYPE TABLE OF t_zmacros_ddtext WITH HEADER LINE,

    it_zmacros_h_t    TYPE TABLE OF t_zmacros_h WITH HEADER LINE,
    it_zmacros_h      TYPE TABLE OF t_zmacros_h WITH HEADER LINE,
    it_zmacros_s      TYPE TABLE OF t_zmacros_s WITH HEADER LINE,
    ret_tab           LIKE ddshretval OCCURS 0 WITH HEADER LINE,
    lv_version        TYPE tdversion,
    lv_id             TYPE f_id.



  PERFORM detect_table.


  IF p_all IS INITIAL.

    SELECT (' BNAME   DDTEXT MAX( VERSION ) AS VERSION    ID')
              FROM ('ZMACROS_H')
              INTO CORRESPONDING FIELDS OF TABLE it_zmacros_h_t
              GROUP BY ('BNAME   DDTEXT   ID')
              ORDER BY ('ID DESCENDING') .

  ELSE.
*>>> all version
    SELECT (' BNAME   DDTEXT  VERSION    ID')
            FROM ('ZMACROS_H')
            INTO CORRESPONDING FIELDS OF TABLE it_zmacros_h_t
            "GROUP BY ('BNAME   DDTEXT   ID')
            ORDER BY ('ID DESCENDING') .
*<<< all version
  ENDIF.

  LOOP AT it_zmacros_h_t.
    MOVE-CORRESPONDING it_zmacros_h_t TO wa_zmacros_ddtext.
    COLLECT wa_zmacros_ddtext INTO it_zmacros_ddtext.
  ENDLOOP.

  IF p_all IS  INITIAL.
*>>last version
    LOOP AT it_zmacros_ddtext.
      CLEAR lv_version .
      LOOP AT it_zmacros_h_t WHERE ddtext = it_zmacros_ddtext-ddtext.
        IF lv_version < it_zmacros_h_t-version.
          lv_version = it_zmacros_h_t-version.
        ENDIF.
      ENDLOOP.

      READ TABLE it_zmacros_h_t WITH KEY ddtext = it_zmacros_ddtext-ddtext version = lv_version.
      APPEND it_zmacros_h_t TO it_zmacros_h.

    ENDLOOP.
*<<last version
  ELSE.
*>> all version

    CLEAR   it_zmacros_h.
    APPEND LINES OF it_zmacros_h_t TO it_zmacros_h.
*<< all version
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ID'
      window_title    = 'Select Query'
      value_org       = 'S'
      multiple_choice = ' '
    TABLES
      value_tab       = it_zmacros_h
      return_tab      = ret_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0.
    LOOP AT ret_tab.
      lv_id  = ret_tab-fieldval .
    ENDLOOP.
  ENDIF.

  IF lv_id IS INITIAL.
    EXIT.
  ENDIF.

  SELECT (' * ')
          INTO CORRESPONDING FIELDS OF TABLE it_zmacros_s
          FROM ('ZMACROS_S')
          WHERE id = lv_id
          ORDER BY ('LINE')
    .

  REFRESH itext_t.

  LOOP AT it_zmacros_s .
    APPEND it_zmacros_s-text TO itext_t.
  ENDLOOP.

  display=>fill_abap_editor( editor = g_text_editor_src
                             text   = itext_t[] ).
  PERFORM refresh.

  lr_table->load( lv_id ).

  MESSAGE 'list' TYPE 'S'.
ENDFORM.


FORM detect_table.
  DATA: lv_count  TYPE i,
        lv_flag_h TYPE c,
        lv_flag_s TYPE c,
        lv_flag   TYPE c.

  DATA: itext TYPE TABLE OF tline-tdline
      .
  lv_flag = lv_flag_h = lv_flag_s = 'X'.

  SELECT COUNT( * )
    FROM dd03l
    INTO lv_count
    WHERE tabname = 'ZMACROS_H'.

  IF sy-subrc NE 0.
    CLEAR: lv_flag,
           lv_flag_h.
  ENDIF.

  SELECT COUNT( * )
    FROM dd03l
    INTO lv_count
    WHERE tabname = 'ZMACROS_S'.

  IF sy-subrc NE 0.
    CLEAR: lv_flag,
           lv_flag_s.
  ENDIF.

  SELECT COUNT( * )
  FROM dd03l
  INTO lv_count
  WHERE tabname = 'ZMACROS_V'.

  IF sy-subrc NE 0.
    CLEAR: lv_flag,
           lv_flag_s.
  ENDIF.




  IF lv_flag IS INITIAL.
    CLEAR itext[].
    APPEND '                      !!! ATTENTION !!!!  ' TO itext.
    APPEND '    ' TO itext.
    APPEND '  to use this aaditional functionality  ' TO itext.
    APPEND '  please create 2 z_table with folowing structure,  ' TO itext.
    APPEND '    ' TO itext.
    APPEND '  all data type alredy exist in ABAP DICTIONARY ' TO itext.
    APPEND '    ' TO itext.
    APPEND '    ' TO itext.
    APPEND '  1) ZMACROS_H  ' TO itext.
    APPEND '    ' TO itext.
    APPEND '  key      field_name                    data_type          ' TO itext.
    APPEND '  ----------------------------------------------------------- ' TO itext.
    APPEND '   X        CLNT                              MANDT           ' TO itext.
    APPEND '   X        BNAME                           XUBNAME         ' TO itext.
    APPEND '   X        DDTEXT                         AS4TEXT        ' TO itext.
    APPEND '   X        VERSION                      TDVERSION      ' TO itext.
    APPEND '   X        ID                                    F_ID          ' TO itext.
    APPEND '    ' TO itext.
    APPEND '    ' TO itext.
    APPEND '  2) ZMACROS_S  ' TO itext.
    APPEND '    ' TO itext.
    APPEND '    ' TO itext.
    APPEND '  key      field_name                    data_type          ' TO itext.
    APPEND '  ----------------------------------------------------------- ' TO itext.
    APPEND '    ' TO itext.
    APPEND '   X        CLNT                              MANDT         ' TO itext.
    APPEND '   X        ID                                     F_ID           ' TO itext.
    APPEND '   X        LINE                               CIFCOUNT       ' TO itext.
    APPEND '             TEXT                          TDLINE ' TO itext.
    APPEND '    ' TO itext.
    APPEND '    ' TO itext.
    APPEND '  3) ZMACROS_V  ' TO itext.
    APPEND '    ' TO itext.
    APPEND '    ' TO itext.
    APPEND '  key      field_name                    data_type          ' TO itext.
    APPEND '  ----------------------------------------------------------- ' TO itext.
    APPEND '    ' TO itext.
    APPEND '   X        CLNT                              MANDT         ' TO itext.
    APPEND '   X        ID                                     F_ID           ' TO itext.
    APPEND '   X        TABNAME                               F_ID       ' TO itext.
    APPEND '   X        ROWNUMBER                               F_ID       ' TO itext.
    APPEND '   X        FIELD                               F_ID       ' TO itext.
    APPEND '             VALUE                          FIELDNAME ' TO itext.






    display=>fill_abap_editor( editor = g_text_editor_dst
                               text   = itext ).

    MESSAGE 'Create Z_table.' TYPE 'E'.

  ENDIF.

ENDFORM. "detect_table


FORM recursive_replace USING key TYPE string
                             line TYPE any
                             src TYPE wcb_tdline_tab
                        CHANGING
                             dst TYPE wcb_tdline_tab.

  REFRESH dst.

  DATA
        : ls_tab TYPE t_xz_table
        , lv_regex TYPE string
        , lv_regex2 TYPE string
        , result_tab TYPE match_result_tab
        , lv_index TYPE i
        , lt_src TYPE wcb_tdline_tab
        , lt_buf TYPE wcb_tdline_tab
        , lt_dest TYPE wcb_tdline_tab
        , lv_key TYPE string
        , lt_regex TYPE TABLE OF t_pair
        .

  lt_src = src.

  IF key NE ''.

    lv_regex = |\\${ key }-(\\d+)#|.

    LOOP AT lt_src ASSIGNING FIELD-SYMBOL(<fs_src>).
      FIND ALL OCCURRENCES OF REGEX lv_regex IN <fs_src> RESULTS result_tab.

      SORT result_tab BY offset DESCENDING.

      REFRESH lt_regex.

      LOOP AT result_tab ASSIGNING FIELD-SYMBOL(<fs_result>).

        READ TABLE  <fs_result>-submatches ASSIGNING FIELD-SYMBOL(<fs_submatch>) INDEX 1.
        lv_index = <fs_src>+<fs_submatch>-offset(<fs_submatch>-length).

        ASSIGN COMPONENT lv_index OF STRUCTURE line TO FIELD-SYMBOL(<fs_r_field>).
        lv_regex2 = |\\${ key }-{ lv_index }#|.

        APPEND INITIAL LINE TO lt_regex ASSIGNING FIELD-SYMBOL(<fs_regex>).
        <fs_regex>-key = lv_regex2.
        <fs_regex>-value  = <fs_r_field>.

      ENDLOOP.


      LOOP AT lt_regex ASSIGNING <fs_regex> .
*        CONDENSE <fs_regex>-value.
        REPLACE ALL OCCURRENCES OF REGEX <fs_regex>-key IN <fs_src> WITH <fs_regex>-value.

      ENDLOOP.

    ENDLOOP.

  ENDIF.


  DATA
        : lv_in_flag TYPE string
        , lv_tabname_key TYPE tabname
        .

  FIELD-SYMBOLS
                 : <fs_data_table> TYPE STANDARD TABLE
                 .

  LOOP AT lt_src ASSIGNING <fs_src>.

    IF lv_in_flag IS INITIAL.
      FIND REGEX '[>$](\d+)' IN <fs_src> RESULTS result_tab.
      IF sy-subrc NE 0.
        APPEND <fs_src> TO dst.
        CONTINUE.
      ELSE.
        READ TABLE result_tab ASSIGNING <fs_result> INDEX 1.
        lv_in_flag = <fs_src>+<fs_result>-offset(<fs_result>-length).
        IF lv_in_flag(1) = '$'.
          APPEND <fs_src> TO lt_buf.
          lv_key = lv_in_flag+1 .
          lv_tabname_key = lv_key.
          READ TABLE lr_table->lt_table INTO ls_tab WITH KEY tabname = lv_tabname_key.
          ASSIGN ls_tab-lt_data->* TO <fs_data_table>.

          LOOP AT <fs_data_table> ASSIGNING FIELD-SYMBOL(<fs_line>).
            PERFORM recursive_replace
                        USING
                           lv_key
                           <fs_line>
                           lt_buf
                        CHANGING
                           lt_dest.

            APPEND LINES OF lt_dest TO dst.


          ENDLOOP.
          REFRESH lt_buf.
          CLEAR lv_in_flag.

          CONTINUE.

        ELSE.
          lv_key = lv_in_flag .
          REPLACE lv_key IN <fs_src> WITH ''.
          TRANSLATE lv_in_flag USING '><'.


          CONTINUE.

        ENDIF.
      ENDIF.

    ENDIF.

    FIND lv_in_flag IN <fs_src>.
    IF sy-subrc NE 0.
      APPEND <fs_src> TO lt_buf.
    ELSE.
      REPLACE lv_in_flag IN <fs_src> WITH ''.



      REPLACE lv_in_flag IN <fs_src> WITH ''.
      lv_key = lv_in_flag+1 .
      lv_tabname_key = lv_key.
      READ TABLE lr_table->lt_table INTO ls_tab WITH KEY tabname = lv_tabname_key.
      ASSIGN ls_tab-lt_data->* TO <fs_data_table>.

      LOOP AT <fs_data_table> ASSIGNING <fs_line>.
        PERFORM recursive_replace
                    USING
                       lv_key
                       <fs_line>
                       lt_buf
                    CHANGING
                       lt_dest.

        APPEND LINES OF lt_dest TO dst.


      ENDLOOP.
      REFRESH lt_buf.
      CLEAR lv_in_flag.


    ENDIF.



  ENDLOOP.

ENDFORM.

*>>>---tool_bar
FORM add_button_group.

  DEFINE m_add_button.
    CALL METHOD cl_gui_toolbar=>fill_buttons_data_table
      EXPORTING
        fcode      = &1
        icon       = &2
        butn_type  = cntb_btype_button
        text       = &3
        quickinfo  = &4
      CHANGING
        data_table = gt_button_group.
  END-OF-DEFINITION.


  m_add_button 'RUN'    icon_release  'Run'     'Run'.

  m_add_button 'REFRESH'    icon_refresh  'Refresh'     'Refresh'.
  m_add_button 'LIST1'    icon_list  'List'     'List'.
  m_add_button 'LIST_ALL'    icon_list  'List all'     'List all'.
  m_add_button 'SAVE1'    icon_system_save  ''     ''.
  m_add_button 'RANGE'    icon_generate  'Range gen'     'Range gen'.



* Add button group to toolbar
  CALL METHOD go_toolbar->add_button_group
    EXPORTING
      data_table = gt_button_group.

ENDFORM.                               " ADD_BUTTON_GROUP
*<<<---tool_bar

CLASS cls_event_handler IMPLEMENTATION.

* Handles method function_selected  for the toolbar control
  METHOD on_function_selected.
    PERFORM check_table.
    CASE fcode.
      WHEN 'RUN'.
        PERFORM do_stuff.
      WHEN 'REFRESH'.
        PERFORM refresh.
      WHEN 'SAVE1'.
        PERFORM save.
      WHEN 'RANGE'.
        PERFORM range.
      WHEN 'LIST1'.
        PERFORM list USING ''.
      WHEN 'LIST_ALL'.
        PERFORM list USING 'all_query'.
    ENDCASE.
  ENDMETHOD.

  "on_function_selected

  METHOD on_context_menu.
    DATA
          : lt_table TYPE TABLE OF string
          .

    CALL METHOD g_text_editor_src->get_selected_text_as_table
      IMPORTING
        table    = lt_table
      EXCEPTIONS
        error_dp = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CHECK  lines( lt_table ) EQ 1.
    READ TABLE lt_table INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_line>).

    CALL METHOD menu->add_function
      EXPORTING
        fcode = 'REPLACE1'
        text  = |\\{ <fs_line> }/|.

    gv_key_1 = <fs_line>.
    gv_value_1 = |\\{ <fs_line> }/|.


    DATA
          : lv_from_line TYPE i
          , itext_t TYPE TABLE OF tline-tdline
          , result_tab TYPE match_result_tab
          , lt_tab_dimension TYPE TABLE OF t_pair
          .


    CALL METHOD g_text_editor_src->get_selection_pos
      IMPORTING
        from_line              = lv_from_line
      EXCEPTIONS
        error_cntl_call_method = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    display=>read_abap_editor( EXPORTING editor = g_text_editor_src
                               IMPORTING text   = itext_t ).

    READ TABLE itext_t INDEX lv_from_line ASSIGNING FIELD-SYMBOL(<fs_line_text>).

    FIND ALL OCCURRENCES OF REGEX '\$(\d+)-(\d+)#' IN <fs_line_text>  RESULTS result_tab.

    LOOP AT result_tab ASSIGNING FIELD-SYMBOL(<fs_result>).

      APPEND INITIAL LINE TO lt_tab_dimension ASSIGNING FIELD-SYMBOL(<fs_dimension>).

      READ TABLE  <fs_result>-submatches ASSIGNING FIELD-SYMBOL(<fs_submatch>) INDEX 1.
      <fs_dimension>-key = <fs_line_text>+<fs_submatch>-offset(<fs_submatch>-length).

      READ TABLE  <fs_result>-submatches ASSIGNING <fs_submatch> INDEX 2.
      <fs_dimension>-value = <fs_line_text>+<fs_submatch>-offset(<fs_submatch>-length).


    ENDLOOP.

    SORT lt_tab_dimension BY key DESCENDING value DESCENDING.


    DATA
          : lv_i TYPE i
          .

    READ TABLE lt_tab_dimension ASSIGNING <fs_dimension> INDEX '1'.

    IF sy-subrc = 0.
      lv_i = <fs_dimension>-value.
      ADD  1 TO lv_i.

      CALL METHOD menu->add_function
        EXPORTING
          fcode = 'REPLACE2'
          text  = |${ <fs_dimension>-key }-{ lv_i }#|.
      gv_value_2 = |${ <fs_dimension>-key }-{ lv_i }#|.



    ENDIF.


    LOOP AT itext_t  ASSIGNING <fs_line_text>.




      FIND ALL OCCURRENCES OF REGEX '\$(\d+)-(\d+)#' IN <fs_line_text>  RESULTS result_tab.

      LOOP AT result_tab ASSIGNING <fs_result>.

        APPEND INITIAL LINE TO lt_tab_dimension ASSIGNING <fs_dimension>.

        READ TABLE  <fs_result>-submatches ASSIGNING <fs_submatch> INDEX 1.
        <fs_dimension>-key = <fs_line_text>+<fs_submatch>-offset(<fs_submatch>-length).

        READ TABLE  <fs_result>-submatches ASSIGNING <fs_submatch> INDEX 2.
        <fs_dimension>-value = <fs_line_text>+<fs_submatch>-offset(<fs_submatch>-length).


      ENDLOOP.

    ENDLOOP.

    SORT lt_tab_dimension BY key DESCENDING value DESCENDING.



    READ TABLE lt_tab_dimension ASSIGNING <fs_dimension> INDEX '1'.

    IF sy-subrc = 0.

      lv_i = <fs_dimension>-key.
      ADD  1 TO lv_i.

      CALL METHOD menu->add_function
        EXPORTING
          fcode = 'REPLACE3'
          text  = |${ lv_i }-1#|.
      gv_value_3 = |${ lv_i }-1#|.


    ELSE.

      CALL METHOD menu->add_function
        EXPORTING
          fcode = 'REPLACE3'
          text  = |$1-1#|.

      gv_value_3   = |$1-1#|.

    ENDIF.

  ENDMETHOD.


  METHOD on_context_menu_selected.
    DATA

    : itext_t TYPE TABLE OF tline-tdline
    .

    display=>read_abap_editor( EXPORTING editor = g_text_editor_src
                               IMPORTING text   = itext_t ).

    FIELD-SYMBOLS
      : <fs_str> TYPE string
      .

    CASE fcode.
      WHEN 'REPLACE1'.
        ASSIGN gv_value_1 TO <fs_str>.
      WHEN 'REPLACE2'.
        ASSIGN gv_value_2 TO <fs_str>.
      WHEN 'REPLACE3'.
        ASSIGN gv_value_3 TO <fs_str>.
    ENDCASE.


    LOOP AT itext_t ASSIGNING FIELD-SYMBOL(<fs_line>).
      REPLACE ALL OCCURRENCES OF gv_key_1 IN <fs_line> WITH <fs_str>.
    ENDLOOP.


    display=>fill_abap_editor( editor = g_text_editor_src
                           text   = itext_t[] ).

  ENDMETHOD.

ENDCLASS.                    "cls_event_handler IMPLEMENTATION

FORM check_table.
  LOOP AT lr_table->lt_table ASSIGNING FIELD-SYMBOL(<fs_xz_refresh>).
    <fs_xz_refresh>-alv->check_changed_data( ).
  ENDLOOP.
ENDFORM.
