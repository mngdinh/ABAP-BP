CLASS lhc_bussinesspartner DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR bussinesspartner RESULT result.

    "Create
    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE bussinesspartner.

    "Update
    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE bussinesspartner.

    "Delete
    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE bussinesspartner.

    METHODS read FOR READ
      IMPORTING keys FOR READ bussinesspartner RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK bussinesspartner.

    "Read Role data
    METHODS rba_role FOR READ
      IMPORTING keys_rba FOR READ bussinesspartner\_role FULL result_requested RESULT result LINK association_links.

    METHODS cba_role FOR MODIFY
      IMPORTING entities_cba FOR CREATE bussinesspartner\_role.

    METHODS rba_addr FOR READ
      IMPORTING keys_rba FOR READ bussinesspartner\_addr FULL result_requested RESULT result LINK association_links.

    METHODS cba_addr FOR MODIFY
      IMPORTING entities_cba FOR CREATE bussinesspartner\_addr.

ENDCLASS.

CLASS lhc_bussinesspartner IMPLEMENTATION.

  METHOD get_instance_authorizations.
    result = VALUE #(
               FOR key IN keys
               ( %tky  = key-%tky
                 %update = if_abap_behv=>auth-allowed
                 %delete = if_abap_behv=>auth-allowed ) ).
  ENDMETHOD.

  METHOD create.

    "bapi bp data
    DATA: ls_head           TYPE bapibus1006_head,
          ls_central        TYPE bapibus1006_central,
          ls_central_person TYPE bapibus1006_central_person,
          ls_central_org    TYPE bapibus1006_central_organ,
          ls_address        TYPE bapibus1006_address,

          "bus_partner number
          lv_bpartner       TYPE bapibus1006_head-bpartner,

          "message
          lt_msg            TYPE TABLE OF bapiret2,
          ls_msg            TYPE bapiret2.
    LOOP AT entities INTO DATA(ls_entity).
      CLEAR: ls_head, ls_central, ls_central_person, ls_central_org, lt_msg.

      ls_head-partn_cat = ls_entity-type.
      ls_head-partn_grp = ls_entity-bugroup.
      ls_central-searchterm1 = ls_entity-busort1.
      ls_central-searchterm2 = ls_entity-busort2.
      ls_central-title_key = ls_entity-title.

      "check type: if 1 then person else org
      IF '1' = ls_entity-type.
        ls_central_person-firstname = ls_entity-namefirst.
        ls_central_person-lastname = ls_entity-namelast.
        ls_central_person-middlename = ls_entity-namemiddle.
      ELSE.
        ls_central_org-name1 = ls_entity-nameorg1.
        ls_central_org-name2 = ls_entity-nameorg2.
      ENDIF.

      "bapi create
      CALL FUNCTION 'BAPI_BUPA_CREATE_FROM_DATA'
        EXPORTING
          partnercategory         = ls_head-partn_cat
          partnergroup            = ls_head-partn_grp
          centraldata             = ls_central
          centraldataperson       = ls_central_person
          centraldataorganization = ls_central_org
        IMPORTING
          businesspartner         = lv_bpartner
        TABLES
          return                  = lt_msg.

      " Check for errors
      IF line_exists( lt_msg[ type = 'E' ] )
            OR line_exists( lt_msg[ type = 'A' ] ).
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
          %cid = ls_entity-%cid
          %msg = new_message_with_text(
                            severity = if_abap_behv_message=>severity-error
                            text     = ls_msg-message )
       ) TO reported-bussinesspartner.
        ENDLOOP.
      ELSE.
        "map bp to rs
        APPEND VALUE #(
            %cid = ls_entity-%cid
            partner = lv_bpartner
         ) TO mapped-bussinesspartner.

        "success msg
        APPEND VALUE #(
            %cid = ls_entity-%cid
            %msg = new_message_with_text(
                                  severity = if_abap_behv_message=>severity-success
                                  text = |Bussiness Partner { lv_bpartner } has been created| )
         ) TO reported-bussinesspartner.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD update.

    DATA: ls_cent     TYPE bapibus1006_central,
          ls_centx    TYPE bapibus1006_central_x,
          ls_pers     TYPE bapibus1006_central_person,
          ls_persx    TYPE bapibus1006_central_person_x,
          ls_org      TYPE bapibus1006_central_organ,
          ls_orgx     TYPE bapibus1006_central_organ_x,
          lv_bp       TYPE bapibus1006_head-bpartner,
          lt_msg      TYPE TABLE OF bapiret2,
          ls_msg      TYPE bapiret2,
          lv_exists   TYPE bapiflag,
          lv_archived TYPE bapibus1006_central-centralarchivingflag.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<e>).

      IF <e>-partner IS INITIAL.
        APPEND VALUE #( %key = VALUE #( partner = <e>-partner ) ) TO failed-bussinesspartner.
        APPEND VALUE #(
          %msg = new_message( id = 'ZBP_BP' number = '010'
                              severity = if_abap_behv_message=>severity-error ) )
          TO reported-bussinesspartner.
        CONTINUE.
      ENDIF.

      "normalize BP
      lv_bp = <e>-partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      "existence
      CLEAR lt_msg.
      DATA(ls_exist) = zcl_bp236_validation=>check_bp_existence( iv_bp = lv_bp ).
      IF ls_exist-exists = abap_false.
        APPEND VALUE #( %key = VALUE #( partner = <e>-partner ) ) TO failed-bussinesspartner.
        APPEND VALUE #(
          %msg = new_message_with_text(
                    severity = if_abap_behv_message=>severity-error
                    text     = ls_exist-error_message )
        ) TO reported-bussinesspartner.
        CONTINUE.
      ENDIF.

      "check archived
      CLEAR: ls_cent, lt_msg.
      DATA(ls_archived) = zcl_bp236_validation=>check_bp_not_archived(
            iv_bp = lv_bp
            iv_action = zcl_bp236_validation=>c_action_update
      ).
      IF ls_archived-is_archived = abap_true.
        APPEND VALUE #( %key = VALUE #( partner = <e>-partner ) ) TO failed-bussinesspartner.
        APPEND VALUE #(
                %msg = new_message_with_text(
                        severity = if_abap_behv_message=>severity-error
                        text     = ls_archived-error_message )
        ) TO reported-role.
        CONTINUE.
      ENDIF.

      "map & X
      CLEAR: ls_centx, ls_pers, ls_persx, ls_org, ls_orgx.
      IF <e>-busort1 IS NOT INITIAL AND ls_cent-searchterm1 <> <e>-busort1.
        ls_cent-searchterm1 = <e>-busort1. ls_centx-searchterm1 = 'X'.
      ENDIF.
      IF <e>-busort2 IS NOT INITIAL AND ls_cent-searchterm2 <> <e>-busort2.
        ls_cent-searchterm2 = <e>-busort2. ls_centx-searchterm2 = 'X'.
      ENDIF.
      IF <e>-title IS NOT INITIAL AND ls_cent-title_key <> <e>-title.
        ls_cent-title_key   = <e>-title.   ls_centx-title_key   = 'X'.
      ENDIF.

      "Cân nhắc: thường KHÔNG đổi category/group; nếu cần, bật X bên dưới.
*       ls_cent-partn_grp   = <e>-bugroup. ls_centx-partn_grp   = 'X'.
*       ls_cent-partn_cat   = <e>-type.    ls_centx-partn_cat   = 'X'.
      SELECT type, name_org1, name_org2, name_last, name_first, namemiddle
      FROM but000
      WHERE partner = @lv_bp
      INTO @DATA(ls_but000).
      ENDSELECT.

      IF '1' = ls_but000-type.
        IF <e>-namefirst IS NOT INITIAL AND <e>-namefirst <> ls_but000-name_first.
          ls_pers-firstname  = <e>-namefirst.  ls_persx-firstname  = 'X'.
        ENDIF.
        IF <e>-namelast IS NOT INITIAL AND <e>-namelast <> ls_but000-name_last.
          ls_pers-lastname   = <e>-namelast.   ls_persx-lastname   = 'X'.
        ENDIF.
        IF <e>-namemiddle IS NOT INITIAL AND <e>-namemiddle <> ls_but000-namemiddle.
          ls_pers-middlename = <e>-namemiddle. ls_persx-middlename = 'X'.
        ENDIF.
      ELSE.
        IF <e>-nameorg1 IS NOT INITIAL AND <e>-nameorg1 <> ls_but000-name_org1.
          ls_org-name1 = <e>-nameorg1. ls_orgx-name1 = 'X'.
        ENDIF.
        IF <e>-nameorg2 IS NOT INITIAL AND <e>-nameorg2 <> ls_but000-name_org2.
          ls_org-name2 = <e>-nameorg2. ls_orgx-name2 = 'X'.
        ENDIF.
      ENDIF.

      "change
      CLEAR lt_msg.
      CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
        EXPORTING
          businesspartner           = lv_bp
          centraldata               = ls_cent
          centraldata_x             = ls_centx
          centraldataperson         = ls_pers
          centraldataperson_x       = ls_persx
          centraldataorganization   = ls_org
          centraldataorganization_x = ls_orgx
        TABLES
          return                    = lt_msg.

      IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = VALUE #( partner = <e>-partner )
            %msg = new_message(
                     id       = ls_msg-id
                     number   = ls_msg-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_msg-message_v1
                     v2       = ls_msg-message_v2
                     v3       = ls_msg-message_v3
                     v4       = ls_msg-message_v4 ) )
          TO reported-bussinesspartner.
        ENDLOOP.
        CONTINUE.
      ENDIF.

      APPEND VALUE #(
        %key = VALUE #( partner = <e>-partner )
        %msg = new_message_with_text(
                 severity = if_abap_behv_message=>severity-success
                 text     = |Business Partner { lv_bp } updated successfully.| ) )
      TO reported-bussinesspartner.

    ENDLOOP.

  ENDMETHOD.

  METHOD delete.

    DATA: lv_bp     TYPE bapibus1006_head-bpartner,
          ls_cent   TYPE bapibus1006_central,
          ls_centx  TYPE bapibus1006_central_x,
          lt_msg    TYPE TABLE OF bapiret2,
          ls_msg    TYPE bapiret2,
          lv_exists TYPE bapiflag.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).

      " validate key
      IF <k>-partner IS INITIAL.
        APPEND VALUE #( %key = VALUE #( partner = <k>-partner ) ) TO failed-bussinesspartner.
        APPEND VALUE #(
          %msg = new_message(
                   id       = 'ZBP_BP'
                   number   = '010'
                   severity = if_abap_behv_message=>severity-error ) )
          TO reported-bussinesspartner.
        CONTINUE.
      ENDIF.

      " normalize BP to 10 chars
      lv_bp = <k>-partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      "Check archived
      CLEAR: ls_cent, lt_msg.
      DATA(ls_archived) = zcl_bp236_validation=>check_bp_not_archived(
                                iv_bp = lv_bp
                                iv_action = zcl_bp236_validation=>c_action_delete
      ).
      IF ls_archived-is_archived = abap_true.
        APPEND VALUE #(
            %key = VALUE #( partner = <k>-partner )
            %msg = new_message_with_text(
                    severity = if_abap_behv_message=>severity-error
                    text = ls_archived-error_message
             ) ) TO reported-bussinesspartner.
      ENDIF.

      " archive via change (set flag + X)
      CLEAR: ls_cent, ls_centx, lt_msg.
      ls_cent-centralarchivingflag  = 'X'.
      ls_centx-centralarchivingflag = 'X'.

      CALL FUNCTION 'BAPI_BUPA_CENTRAL_CHANGE'
        EXPORTING
          businesspartner = lv_bp
          centraldata     = ls_cent
          centraldata_x   = ls_centx
        TABLES
          return          = lt_msg.

      IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = VALUE #( partner = <k>-partner )
            %msg = new_message(
                     id       = ls_msg-id
                     number   = ls_msg-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_msg-message_v1
                     v2       = ls_msg-message_v2
                     v3       = ls_msg-message_v3
                     v4       = ls_msg-message_v4 ) )
            TO reported-bussinesspartner.
        ENDLOOP.
        CONTINUE.
      ENDIF.

      APPEND VALUE #(
        %key = VALUE #( partner = <k>-partner )
        %msg = new_message_with_text(
                 severity = if_abap_behv_message=>severity-success
                 text     = |Business Partner { lv_bp } archived successfully.| ) )
        TO reported-bussinesspartner.

    ENDLOOP.

  ENDMETHOD.

  METHOD read.

    DATA: lt_msg TYPE TABLE OF bapiret2,
          ls_det TYPE bapibus1006_central,
          lv_bp  TYPE bapibus1006_head-bpartner.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).

      IF <k>-partner IS INITIAL.
        CONTINUE.
      ENDIF.

      lv_bp = <k>-partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      "lấy central để biết có archived không
      CLEAR: ls_det, lt_msg.
      CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
        EXPORTING
          businesspartner = lv_bp
        IMPORTING
          centraldata     = ls_det
        TABLES
          return          = lt_msg.

      "bỏ qua archived (nếu muốn vẫn trả về thì bỏ điều kiện này)
      IF ls_det-centralarchivingflag = 'X'.
        CONTINUE.
      ENDIF.

      "đọc BUT000 để map trả về như cũ  (KHÔNG cần xdele nữa)
      SELECT SINGLE partner, type, bu_group, bu_sort1, bu_sort2, title,
                     name_org1, name_org2, name_last, name_first, namemiddle,
                     crusr, crdat, crtim, chusr, chdat, chtim
        FROM but000
        WHERE partner = @lv_bp
        INTO @DATA(ls_bu).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      DATA lv_del TYPE c LENGTH 1.
      IF ls_det-centralarchivingflag = 'X'.
        lv_del = 'X'.
      ELSE.
        CLEAR lv_del.
      ENDIF.


      "KHÔNG double-check xdele nữa – ta tin theo centralarchivingflag
*       IF ls_bu-xdele = 'X'.
*         CONTINUE.
*       ENDIF.

      "CHANGED: map deletionflag từ centralarchivingflag (BAPI)
      APPEND VALUE #(
        partner      = ls_bu-partner
        type         = ls_bu-type
        bugroup      = ls_bu-bu_group
        busort1      = ls_bu-bu_sort1
        busort2      = ls_bu-bu_sort2
        title        = ls_bu-title
        nameorg1     = ls_bu-name_org1
        nameorg2     = ls_bu-name_org2
        namelast     = ls_bu-name_last
        namefirst    = ls_bu-name_first
        namemiddle   = ls_bu-namemiddle
        crusr        = ls_bu-crusr
        crdat        = ls_bu-crdat
        crtim        = ls_bu-crtim
        chusr        = ls_bu-chusr
        chdat        = ls_bu-chdat
        chtim        = ls_bu-chtim
        deletionflag = lv_del )
      TO result.

    ENDLOOP.

  ENDMETHOD.

  METHOD lock.
  ENDMETHOD.

  "Đọc danh sách Role của một Business Partner
  METHOD rba_role.
    TYPES: BEGIN OF ty_bp,
             partner TYPE bu_partner,
           END OF ty_bp.

    DATA: lt_req    TYPE STANDARD TABLE OF ty_bp,
          lt_active TYPE STANDARD TABLE OF ty_bp,
          lt_role   TYPE TABLE OF but100,
          ls_role   TYPE but100.

    "1) Chuẩn hoá key -> 10 ký tự & gom yêu cầu
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<k>).
      IF <k>-partner IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(lv_bp) = <k>-partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      APPEND VALUE ty_bp( partner = lv_bp ) TO lt_req.
    ENDLOOP.

    IF lt_req IS INITIAL.
      RETURN.
    ENDIF.

    "Loại trùng
    SORT lt_req BY partner.
    DELETE ADJACENT DUPLICATES FROM lt_req COMPARING partner.

    "2) Lọc BP còn active (không archived)
    SELECT partner
      FROM but000
      FOR ALL ENTRIES IN @lt_req
      WHERE partner = @lt_req-partner
        AND xdele   <> 'X'
      INTO TABLE @lt_active.

    IF sy-subrc <> 0 OR lt_active IS INITIAL.
      RETURN.
    ENDIF.

    "3) Lấy roles từ BUT100 (NHỚ dấu phẩy giữa các cột)
    SELECT partner,
           rltyp,
           dfval,
           valid_from,
           valid_to
      FROM but100
      FOR ALL ENTRIES IN @lt_active
      WHERE partner = @lt_active-partner
      INTO CORRESPONDING FIELDS OF TABLE @lt_role.

    IF sy-subrc <> 0 OR lt_role IS INITIAL.
      RETURN.
    ENDIF.

    "4) Trả kết quả
    LOOP AT lt_role INTO ls_role.
      APPEND VALUE #(
        partner   = ls_role-partner
        rltyp     = ls_role-rltyp
        dfval     = ls_role-dfval
        validfrom = ls_role-valid_from
        validto   = ls_role-valid_to ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD cba_role.

    DATA: lv_bpartner   TYPE bapibus1006_head-bpartner,
          lv_role       TYPE bapibus1006_bproles-partnerrole,
          lv_dfval      TYPE bapibus1006_bproles-difftypevalue,
          lv_from       TYPE bapibus1006_bprole_validity-bprolevalidfrom,
          lv_to         TYPE bapibus1006_bprole_validity-bprolevalidto,
          lv_addressnum TYPE adrc-addrnumber,

          "message
          lt_msg        TYPE TABLE OF bapiret2,
          ls_msg        TYPE bapiret2.

    LOOP AT entities_cba INTO DATA(ls_entity).

      lv_bpartner = ls_entity-partner.

      "Convert BP to 10 char
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bpartner
        IMPORTING
          output = lv_bpartner.

      "check archived
      DATA(ls_archived) = zcl_bp236_validation=>check_bp_not_archived(
                                            iv_bp = lv_bpartner
                                            iv_action = zcl_bp236_validation=>c_action_create ).
      IF ls_archived-is_archived = abap_true.
        APPEND VALUE #(
            %key = ls_entity-%key
            %msg = new_message_with_text(
                     severity = if_abap_behv_message=>severity-error
                     text = ls_archived-error_message
             ) ) TO reported-role.
        CONTINUE.
      ENDIF.

      "check address exists or not
      DATA(ls_address) = zcl_bp236_validation=>check_bp_address( iv_bp = lv_bpartner ).
      IF ls_address-exists = abap_false.
        APPEND VALUE #(
            %key = ls_entity-%key
            %msg = new_message_with_text(
                     severity = if_abap_behv_message=>severity-error
                     text     = |Create Address for BP First.| )
          ) TO reported-role.
        CONTINUE.
      ENDIF.

      LOOP AT ls_entity-%target INTO DATA(ls_role).
        lv_role = ls_role-rltyp.
        lv_dfval = ls_role-dfval.

        "validfrom || validto is filled
        "if from is null then sy-datum else filed
        FIELD-SYMBOLS <t_from> TYPE any.
        ASSIGN COMPONENT 'VALIDFROM' OF STRUCTURE ls_role TO <t_from>.
        IF sy-subrc = 0 AND <t_from> IS NOT INITIAL.
          lv_from = <t_from>.
        ELSE.
          lv_from = sy-datum.
        ENDIF.

        "validto
        FIELD-SYMBOLS <t_to> TYPE any.
        ASSIGN COMPONENT 'VALIDTO' OF STRUCTURE ls_role TO <t_to>.
        IF sy-subrc = 0 AND <t_to> IS NOT INITIAL.
          lv_to = <t_to>.
        ELSE.
          lv_to = '99991231'.
        ENDIF.

        IF lv_to < lv_from.
          lv_to = lv_from.
        ENDIF.

        CLEAR lt_msg.

        "bapi role
        CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
          EXPORTING
            businesspartner          = lv_bpartner
            businesspartnerrole      = lv_role
            differentiationtypevalue = lv_dfval
            validfromdate            = lv_from
            validuntildate           = lv_to
          TABLES
            return                   = lt_msg.

        "error msg
        IF line_exists( lt_msg[ type = 'E' ] )
            OR line_exists( lt_msg[ type = 'A' ] ).

          LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
            APPEND VALUE #(
              %key = ls_entity-%key
              %msg = new_message(
                       id       = ls_msg-id
                       number   = ls_msg-number
                       severity = if_abap_behv_message=>severity-error
                       v1       = ls_msg-message_v1
                       v2       = ls_msg-message_v2
                       v3       = ls_msg-message_v3
                       v4       = ls_msg-message_v4 )
            ) TO reported-role. "response
          ENDLOOP.
        ELSE.
          APPEND VALUE #(
          %cid        = ls_role-%cid
          partner     = lv_bpartner
          rltyp       = lv_role
          dfval       = lv_dfval
                  ) TO mapped-role.

          APPEND VALUE #(
            %key = ls_entity-%key
            %msg = new_message_with_text(
                     severity = if_abap_behv_message=>severity-success
                     text     = |Role { lv_role } has added to BP { lv_bpartner }.| )
          ) TO reported-role.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD rba_addr.

    TYPES: BEGIN OF ty_bp,
             partner TYPE bu_partner,
           END OF ty_bp.

    DATA: lt_req    TYPE STANDARD TABLE OF ty_bp,
          lt_bp_act TYPE STANDARD TABLE OF ty_bp.

    " Gom & chuẩn hoá Partner
    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<k>).
      IF <k>-partner IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA(lv_bp) = <k>-partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.
      APPEND VALUE ty_bp( partner = lv_bp ) TO lt_req.
    ENDLOOP.

    IF lt_req IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_req BY partner.
    DELETE ADJACENT DUPLICATES FROM lt_req COMPARING partner.

    " (Tuỳ chọn) Lọc BP còn active để đồng nhất với read
    SELECT partner
      FROM but000
      FOR ALL ENTRIES IN @lt_req
      WHERE partner = @lt_req-partner
        AND xdele   <> 'X'
      INTO TABLE @lt_bp_act.

    IF sy-subrc <> 0 OR lt_bp_act IS INITIAL.
      RETURN.
    ENDIF.

    " Lấy địa chỉ, alias đúng projection; map bằng CORRESPONDING cho an toàn
    SELECT
      b~partner                      AS partner,
      b~addrnumber                   AS addrnumber,
      a~country                      AS country,
      a~region                       AS region,
      a~post_code1                   AS postalcode,
      a~street                       AS street,
      a~city1                        AS city,
      a~name_co                      AS namecompany,
      a~house_num1                   AS housenumber
    FROM but020 AS b
    INNER JOIN adrc  AS a ON a~addrnumber = b~addrnumber
    FOR ALL ENTRIES IN @lt_bp_act
    WHERE b~partner = @lt_bp_act-partner
    INTO TABLE @DATA(lt_addr).

    IF sy-subrc <> 0 OR lt_addr IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_addr ASSIGNING FIELD-SYMBOL(<r>).
      APPEND CORRESPONDING #( <r> ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD cba_addr.

    DATA: lv_bp       TYPE bapibus1006_head-bpartner,
          ls_cent     TYPE bapibus1006_central,
          lt_msg      TYPE TABLE OF bapiret2,
          ls_msg      TYPE bapiret2,
          ls_bapiaddr TYPE bapibus1006_address,
          lv_addrno   TYPE ad_addrnum.

    FIELD-SYMBOLS:
      <f_country>     TYPE any,
      <f_region>      TYPE any,
      <f_postalcode>  TYPE any,
      <f_street>      TYPE any,
      <f_city>        TYPE any,
      <f_namecompany> TYPE any,
      <f_housenumber> TYPE any.

    LOOP AT entities_cba INTO DATA(ls_parent).

      " Partner cha
      lv_bp = ls_parent-partner.
      IF lv_bp IS INITIAL.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      "check archived
      DATA(ls_archived) = zcl_bp236_validation=>check_bp_not_archived(
            iv_bp = lv_bp
            iv_action = zcl_bp236_validation=>c_action_create
      ).
      IF ls_archived-is_archived = abap_true.
        APPEND VALUE #( %key = VALUE #( partner = ls_parent-partner ) ) TO failed-bussinesspartner.
        APPEND VALUE #(
                %msg = new_message_with_text(
                        severity = if_abap_behv_message=>severity-error
                        text     = ls_archived-error_message )
        ) TO reported-role.
        CONTINUE.
      ENDIF.

      " Duyệt từng địa chỉ con
      LOOP AT ls_parent-%target INTO DATA(ls_child).

        CLEAR: ls_bapiaddr, lt_msg, lv_addrno.

        " Đọc động các field từ %target (an toàn compile nếu field vắng mặt)
        ASSIGN COMPONENT 'COUNTRY'     OF STRUCTURE ls_child TO <f_country>.
        ASSIGN COMPONENT 'REGION'      OF STRUCTURE ls_child TO <f_region>.
        ASSIGN COMPONENT 'POSTALCODE'  OF STRUCTURE ls_child TO <f_postalcode>.
        ASSIGN COMPONENT 'STREET'      OF STRUCTURE ls_child TO <f_street>.
        ASSIGN COMPONENT 'CITY'        OF STRUCTURE ls_child TO <f_city>.
        ASSIGN COMPONENT 'NAMECOMPANY' OF STRUCTURE ls_child TO <f_namecompany>.
        ASSIGN COMPONENT 'HOUSENUMBER' OF STRUCTURE ls_child TO <f_housenumber>.

        IF <f_country>     IS ASSIGNED. ls_bapiaddr-country    = <f_country>.     ENDIF.
        IF <f_region>      IS ASSIGNED. ls_bapiaddr-region     = <f_region>.      ENDIF.
        IF <f_postalcode>  IS ASSIGNED. ls_bapiaddr-postl_cod1 = <f_postalcode>.  ENDIF.
        IF <f_street>      IS ASSIGNED. ls_bapiaddr-street     = <f_street>.      ENDIF.
        IF <f_city>        IS ASSIGNED. ls_bapiaddr-city       = <f_city>.        ENDIF.
        IF <f_namecompany> IS ASSIGNED. ls_bapiaddr-c_o_name   = <f_namecompany>. ENDIF.
        IF <f_housenumber> IS ASSIGNED. ls_bapiaddr-house_no   = <f_housenumber>. ENDIF.

        ls_bapiaddr-langu       = sy-langu.
        ls_bapiaddr-validfromdate  = sy-datum.
        ls_bapiaddr-validtodate = '99991231'.

        " Gọi BAPI tạo địa chỉ
        CLEAR lt_msg.
        CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
          EXPORTING
            businesspartner = lv_bp
            addressdata     = ls_bapiaddr
          TABLES
            return          = lt_msg.


        IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
          LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
            APPEND VALUE #(
              %key = ls_parent-%key
              %msg = new_message(
                       id       = ls_msg-id
                       number   = ls_msg-number
                       severity = if_abap_behv_message=>severity-error
                       v1       = ls_msg-message_v1
                       v2       = ls_msg-message_v2
                       v3       = ls_msg-message_v3
                       v4       = ls_msg-message_v4 ) )
            TO reported-addr.
          ENDLOOP.
          CONTINUE.
        ENDIF.

        " Fallback: nếu hệ không trả addressnumber thì lấy MAX(addrnumber)
        IF lv_addrno IS INITIAL.
          SELECT MAX( addrnumber )
            FROM but020
            WHERE partner = @lv_bp
            INTO @lv_addrno.
        ENDIF.

        " Trả mapped (child)
        APPEND VALUE #(
          %cid       = ls_child-%cid
          partner    = lv_bp
          addrnumber = lv_addrno ) TO mapped-addr.

        " Success message
        APPEND VALUE #(
          %key = ls_parent-%key
          %msg = new_message_with_text(
                   severity = if_abap_behv_message=>severity-success
                   text     = |Address { lv_addrno } has been added to BP { lv_bp }.| ) )
        TO reported-addr.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

CLASS lhc_role DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE role.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE role.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE role.

    METHODS read FOR READ
      IMPORTING keys FOR READ role RESULT result.

    METHODS rba_root FOR READ
      IMPORTING keys_rba FOR READ role\_root FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_role IMPLEMENTATION.

  METHOD create.
    DATA: lv_bpartner TYPE bu_partner,
          lv_role     TYPE bu_role,
          lv_dfval    TYPE bu_dfval,
          lv_from     TYPE bu_valid_from,
          lv_to       TYPE bu_valid_to,

          "message
          lt_msg      TYPE TABLE OF bapiret2,
          ls_msg      TYPE bapiret2.

    LOOP AT entities INTO DATA(ls_entity).

      lv_bpartner = ls_entity-partner.

      "Convert BP to 10 char
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bpartner
        IMPORTING
          output = lv_bpartner.

      "check archived
      DATA(ls_archived) = zcl_bp236_validation=>check_bp_not_archived(
            iv_bp = lv_bpartner
            iv_action = zcl_bp236_validation=>c_action_create
      ).
      IF ls_archived-is_archived = abap_true.
        APPEND VALUE #( %key = VALUE #( partner = ls_entity-partner ) ) TO failed-bussinesspartner.
        APPEND VALUE #(
                %msg = new_message_with_text(
                        severity = if_abap_behv_message=>severity-error
                        text     = ls_archived-error_message )
        ) TO reported-role.
        CONTINUE.
      ENDIF.

      "check address exists or not
      DATA(ls_address) = zcl_bp236_validation=>check_bp_address( iv_bp = lv_bpartner ).
      IF ls_address-exists = abap_false.
        APPEND VALUE #(
            %key = ls_entity-%key
            %msg = new_message_with_text(
                     severity = if_abap_behv_message=>severity-error
                     text     = |Create Address for BP First.| )
          ) TO reported-role.
        CONTINUE.
      ENDIF.

      lv_role = ls_entity-rltyp.
      lv_dfval = ls_entity-dfval.

      "validfrom || validto is filled
      "if from is null then sy-datum else filed
      FIELD-SYMBOLS <t_from> TYPE any.
      ASSIGN COMPONENT 'VALIDFROM' OF STRUCTURE ls_entity TO <t_from>.
      IF sy-subrc = 0 AND <t_from> IS NOT INITIAL.
        lv_from = <t_from>.
      ELSE.
        lv_from = sy-datum.
      ENDIF.

      "validto
      FIELD-SYMBOLS <t_to> TYPE any.
      ASSIGN COMPONENT 'VALIDTO' OF STRUCTURE ls_entity TO <t_to>.
      IF sy-subrc = 0 AND <t_to> IS NOT INITIAL.
        lv_to = <t_to>.
      ELSE.
        lv_to = '99991231'.
      ENDIF.

      IF lv_to < lv_from.
        lv_to = lv_from.
      ENDIF.

      CLEAR lt_msg.

      "bapi role
      CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
        EXPORTING
          businesspartner          = lv_bpartner
          businesspartnerrole      = lv_role
          differentiationtypevalue = lv_dfval
          validfromdate            = lv_from
          validuntildate           = lv_to
        TABLES
          return                   = lt_msg.

      "error msg
      IF line_exists( lt_msg[ type = 'E' ] )
          OR line_exists( lt_msg[ type = 'A' ] ).

        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = ls_entity-%key
            %msg = new_message(
                     id       = ls_msg-id
                     number   = ls_msg-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_msg-message_v1
                     v2       = ls_msg-message_v2
                     v3       = ls_msg-message_v3
                     v4       = ls_msg-message_v4 )
          ) TO reported-role. "response
        ENDLOOP.
      ELSE.
        APPEND VALUE #(
        %cid        = ls_entity-%cid
        partner     = lv_bpartner
        rltyp       = lv_role
        dfval       = lv_dfval
                ) TO mapped-role.

        APPEND VALUE #(
          %key = ls_entity-%key
          %msg = new_message_with_text(
                   severity = if_abap_behv_message=>severity-success
                   text     = |Role { lv_role } has added to BP { lv_bpartner }.| )
        ) TO reported-role.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD update.
    DATA: lv_bp        TYPE bapibus1006_head-bpartner,
          lv_role      TYPE bu_role,
          lv_dfval_new TYPE bu_dfval,
          lv_from_new  TYPE bapibus1006_bprole_validity-bprolevalidfrom,
          lv_to_new    TYPE bapibus1006_bprole_validity-bprolevalidto,
          lv_dfval_old TYPE bu_dfval,
          ls_cent      TYPE bapibus1006_central,
          lt_msg       TYPE STANDARD TABLE OF bapiret2,
          ls_msg       TYPE bapiret2.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<e>).

      IF <e>-partner IS INITIAL OR <e>-rltyp IS INITIAL.
        APPEND VALUE #( %key = VALUE #( partner = <e>-partner rltyp = <e>-rltyp ) )
          TO failed-role.
        APPEND VALUE #(
          %msg = new_message(
               id       = 'ZBP_ROLE'
               number   = '001'
               severity = if_abap_behv_message=>severity-error ) )
          TO reported-role.
        CONTINUE.
      ENDIF.

      lv_bp   = <e>-partner.
      lv_role = <e>-rltyp.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      "check bp is archived or not
      CLEAR: ls_cent, lt_msg.
      DATA(ls_check) = zcl_bp236_validation=>check_bp_not_archived(
                    iv_bp     = lv_bp
                    iv_action = zcl_bp236_validation=>c_action_update ).

      IF ls_check-is_archived = abap_true.
        APPEND VALUE #(
            %key = VALUE #( partner = <e>-partner rltyp = <e>-rltyp )
        ) TO failed-role.

        APPEND VALUE #(
          %msg = new_message_with_text(
                    severity = if_abap_behv_message=>severity-error
                    text     = ls_check-error_message )
        ) TO reported-role.
        CONTINUE.
      ENDIF.

      SELECT SINGLE dfval
        FROM but100
        WHERE partner = @lv_bp
          AND rltyp   = @lv_role
        INTO @lv_dfval_old.
      IF sy-subrc <> 0.
        APPEND VALUE #( %key = VALUE #( partner = <e>-partner rltyp = <e>-rltyp ) )
          TO failed-role.

        APPEND VALUE #(
          %msg = new_message(
                   id       = 'ZBP_ROLE'
                   number   = '009'
                   severity = if_abap_behv_message=>severity-error
                   v1       = |{ <e>-partner }|
                   v2       = |{ <e>-rltyp }| ) )
          TO reported-role.

        CONTINUE.
      ENDIF.

      "--- get new payload (safe defaults)
      lv_dfval_new = <e>-dfval.
      DATA(lv_frnew_str) = <e>-validfrom.
      DATA(lv_tonew_str) = <e>-validto.

      IF <e>-validfrom IS INITIAL.
        lv_from_new = sy-datum.
      ELSE.
        lv_from_new = lv_frnew_str(8).
      ENDIF.

      IF <e>-validto IS INITIAL.
        lv_to_new = '99991231'.
      ELSE.
        lv_to_new = lv_tonew_str(8).
      ENDIF.

      IF lv_to_new < lv_from_new.
        lv_to_new = lv_from_new.
      ENDIF.

      CLEAR: lt_msg.

      CALL FUNCTION 'BAPI_BUPA_ROLE_CHANGE'
        EXPORTING
          businesspartner          = lv_bp
          businesspartnerrole      = lv_role
          differentiationtypevalue = lv_dfval_new
          validfromdate            = lv_from_new
          validuntildate           = lv_to_new
        TABLES
          return                   = lt_msg.

      IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %tky = <e>-%tky
            %msg = new_message(
              id       = ls_msg-id
              number   = ls_msg-number
              severity = if_abap_behv_message=>severity-error
              v1       = ls_msg-message_v1
              v2       = ls_msg-message_v2
              v3       = ls_msg-message_v3
              v4       = ls_msg-message_v4 )
          ) TO reported-role.
        ENDLOOP.
      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.

*      " delete old
      CLEAR lt_msg.
      CALL FUNCTION 'BAPI_BUPA_ROLE_REMOVE'
        EXPORTING
          businesspartner     = lv_bp
          businesspartnerrole = lv_role
        TABLES
          return              = lt_msg.

      IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = VALUE #( partner = <e>-partner rltyp = <e>-rltyp )
            %msg = new_message(
                     id       = ls_msg-id
                     number   = ls_msg-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_msg-message_v1
                     v2       = ls_msg-message_v2
                     v3       = ls_msg-message_v3
                     v4       = ls_msg-message_v4 ) )
          TO reported-role.
        ENDLOOP.
        CONTINUE.
      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.

      " add new
      CLEAR lt_msg.
      CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
        EXPORTING
          businesspartner          = lv_bp
          businesspartnerrole      = lv_role
          differentiationtypevalue = lv_dfval_new
          validfromdate            = lv_from_new
          validuntildate           = lv_to_new
        TABLES
          return                   = lt_msg.

      IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = VALUE #( partner = <e>-partner rltyp = <e>-rltyp )
            %msg = new_message(
                     id       = ls_msg-id
                     number   = ls_msg-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_msg-message_v1
                     v2       = ls_msg-message_v2
                     v3       = ls_msg-message_v3
                     v4       = ls_msg-message_v4 ) )
          TO reported-role.
        ENDLOOP.
        CONTINUE.
      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.

      APPEND VALUE #(
        %key = VALUE #( partner = <e>-partner rltyp = <e>-rltyp )
        %msg = new_message_with_text(
                 severity = if_abap_behv_message=>severity-success
                 text     = |Role { lv_role } updated for BP { lv_bp }.| ) )
      TO reported-role.
    ENDLOOP.
  ENDMETHOD.

  METHOD delete.

    DATA: lv_bp    TYPE bapibus1006_head-bpartner,
          lv_role  TYPE bu_role,
          lv_dfval TYPE bu_dfval,
          ls_cent  TYPE bapibus1006_central,
          lt_msg   TYPE TABLE OF bapiret2,
          ls_msg   TYPE bapiret2.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).

      IF <k>-partner IS INITIAL OR <k>-rltyp IS INITIAL.
        APPEND VALUE #( %key = VALUE #( partner = <k>-partner rltyp = <k>-rltyp ) )
          TO failed-role.
        APPEND VALUE #(
          %msg = new_message(
                   id       = 'ZBP_ROLE'
                   number   = '001'
                   severity = if_abap_behv_message=>severity-error ) )
          TO reported-role.
        CONTINUE.
      ENDIF.

      lv_bp   = <k>-partner.
      lv_role = <k>-rltyp.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      "check bp is archived or not
      CLEAR: ls_cent, lt_msg.
      DATA(ls_check) = zcl_bp236_validation=>check_bp_not_archived(
                    iv_bp     = lv_bp
                    iv_action = zcl_bp236_validation=>c_action_delete ).

      IF ls_check-is_archived = abap_true.
        APPEND VALUE #(
            %key = VALUE #( partner = <k>-partner rltyp = <k>-rltyp )
        ) TO failed-role.

        APPEND VALUE #(
          %msg = new_message_with_text(
                    severity = if_abap_behv_message=>severity-error
                    text     = ls_check-error_message )
        ) TO reported-role.
        CONTINUE.
      ENDIF.

      " read dfval (nếu hệ yêu cầu khi delete)
      SELECT SINGLE dfval
        FROM but100
        WHERE partner = @lv_bp
          AND rltyp   = @lv_role
        INTO @lv_dfval.
      IF sy-subrc <> 0.
        APPEND VALUE #( %key = VALUE #( partner = <k>-partner rltyp = <k>-rltyp ) )
          TO failed-role.
        APPEND VALUE #(
          %msg = new_message(
                   id       = 'ZBP_ROLE'
                   number   = '009'
                   severity = if_abap_behv_message=>severity-error
                   v1       = <k>-partner
                   v2       = <k>-rltyp ) )
          TO reported-role.
        CONTINUE.
      ENDIF.

      CLEAR lt_msg.
      CALL FUNCTION 'BAPI_BUPA_ROLE_REMOVE'
        EXPORTING
          businesspartner          = lv_bp
          businesspartnerrole      = lv_role
          differentiationtypevalue = lv_dfval
        TABLES
          return                   = lt_msg.


      IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = VALUE #( partner = <k>-partner rltyp = <k>-rltyp )
            %msg = new_message(
                     id       = ls_msg-id
                     number   = ls_msg-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_msg-message_v1
                     v2       = ls_msg-message_v2
                     v3       = ls_msg-message_v3
                     v4       = ls_msg-message_v4 ) )
          TO reported-role.
        ENDLOOP.
        CONTINUE.
      ENDIF.

      APPEND VALUE #(
        %key = VALUE #( partner = <k>-partner rltyp = <k>-rltyp )
        %msg = new_message_with_text(
                 severity = if_abap_behv_message=>severity-success
                 text     = |Role { lv_role } deleted for BP { lv_bp }.| ) )
      TO reported-role.

    ENDLOOP.

  ENDMETHOD.

  METHOD read.

    " Lấy theo keys, và (khuyến nghị) chỉ trả role của BP chưa archived:
    " dùng join nhanh qua BUT000-xdele <> 'X' để tránh gọi BAPI trong vòng lặp.

    SELECT r~partner,
           r~rltyp,
           r~dfval,
           r~valid_from,
           r~valid_to
      FROM but100 AS r
      INNER JOIN but000 AS b
        ON b~partner = r~partner
     FOR ALL ENTRIES IN @keys
     WHERE r~partner = @keys-partner
       AND r~rltyp   = @keys-rltyp
       AND b~xdele  <> 'X'
      INTO TABLE @DATA(lt_role).

    LOOP AT lt_role ASSIGNING FIELD-SYMBOL(<r>).
      APPEND VALUE #(
        partner   = <r>-partner
        rltyp     = <r>-rltyp
        dfval     = <r>-dfval
        validfrom = <r>-valid_from
        validto   = <r>-valid_to )
      TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD rba_root.

    DATA: lv_bp  TYPE bapibus1006_head-bpartner,
          lt_msg TYPE TABLE OF bapiret2,
          ls_det TYPE bapibus1006_central.

    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<k>).

      IF <k>-partner IS INITIAL.
        CONTINUE.
      ENDIF.

      lv_bp = <k>-partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      " Lấy central flag để map deletionflag chuẩn từ BAPI
      CLEAR: ls_det, lt_msg.
      CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
        EXPORTING
          businesspartner = lv_bp
        IMPORTING
          centraldata     = ls_det
        TABLES
          return          = lt_msg.

      " Đọc BUT000 để map các field còn lại
      SELECT SINGLE partner, type, bu_group, bu_sort1, bu_sort2, title,
                     name_org1, name_org2, name_last, name_first, namemiddle,
                     crusr, crdat, crtim, chusr, chdat, chtim
        FROM but000
        WHERE partner = @lv_bp
        INTO @DATA(ls_bu).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      DATA lv_del TYPE c LENGTH 1.
      IF ls_det-centralarchivingflag = 'X'.
        lv_del = 'X'.
      ELSE.
        CLEAR lv_del.
      ENDIF.

      APPEND VALUE #(
        partner      = ls_bu-partner
        type         = ls_bu-type
        bugroup      = ls_bu-bu_group
        busort1      = ls_bu-bu_sort1
        busort2      = ls_bu-bu_sort2
        title        = ls_bu-title
        nameorg1     = ls_bu-name_org1
        nameorg2     = ls_bu-name_org2
        namelast     = ls_bu-name_last
        namefirst    = ls_bu-name_first
        namemiddle   = ls_bu-namemiddle
        crusr        = ls_bu-crusr
        crdat        = ls_bu-crdat
        crtim        = ls_bu-crtim
        chusr        = ls_bu-chusr
        chdat        = ls_bu-chdat
        chtim        = ls_bu-chtim
        deletionflag = lv_del )
      TO result.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS lhc_addr DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE addr.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE addr.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE addr.

    METHODS read FOR READ
      IMPORTING keys FOR READ addr RESULT result.

    METHODS rba_root FOR READ
      IMPORTING keys_rba FOR READ addr\_root FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_addr IMPLEMENTATION.

  METHOD create.

    DATA:
      lt_return   TYPE TABLE OF bapiret2,
      ls_return   TYPE bapiret2,
      ls_addrdata TYPE bapibus1006_address.

    LOOP AT entities INTO DATA(ls_cba).

      CLEAR ls_addrdata.

      "Mapping từ RAP entity sang BAPI structure
      ls_addrdata-country     = ls_cba-country.
      ls_addrdata-region      = ls_cba-region.
      ls_addrdata-postl_cod1  = ls_cba-postalcode.
      ls_addrdata-street      = ls_cba-street.
      ls_addrdata-city        = ls_cba-city.
      ls_addrdata-house_no    = ls_cba-housenumber.
      ls_addrdata-c_o_name    = ls_cba-namecompany.
      ls_addrdata-langu       = sy-langu.
      ls_addrdata-validfromdate  = sy-datum.
      ls_addrdata-validtodate = '99991231'.

      "Gọi BAPI
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_ADD'
        EXPORTING
          businesspartner = ls_cba-partner
          addressdata     = ls_addrdata
        TABLES
          return          = lt_return.

      IF line_exists( lt_return[ type = 'E' ] )
           OR line_exists( lt_return[ type = 'A' ] ).
        LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = ls_cba-%key
            %msg = new_message(
                     id       = ls_return-id
                     number   = ls_return-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_return-message_v1
                     v2       = ls_return-message_v2
                     v3       = ls_return-message_v3
                     v4       = ls_return-message_v4 )
          ) TO reported-addr. "response
        ENDLOOP.

      ELSE.

        SELECT SINGLE addrnumber
            FROM but020
            WHERE partner = @ls_cba-partner
            INTO @DATA(lv_addrnumber).

        APPEND VALUE #(
        %cid        = ls_cba-%cid
        partner     = ls_cba-partner
        addrnumber  = lv_addrnumber
                ) TO mapped-addr.

        APPEND VALUE #(
          %key = ls_cba-%key
          %msg = new_message_with_text(
                   severity = if_abap_behv_message=>severity-success
                   text     = |Address has added to BP { ls_cba-partner }.| )
        ) TO reported-addr.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD update.

    DATA: lv_bp     TYPE bapibus1006_head-bpartner,
          lv_addrno TYPE ad_addrnum,
          ls_cent   TYPE bapibus1006_central,
          lt_msg    TYPE TABLE OF bapiret2,
          ls_msg    TYPE bapiret2,
          ls_addr   TYPE bapibus1006_address,
          ls_addrx  TYPE bapibus1006_address_x.

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<e>).

      " validate key
      IF <e>-partner IS INITIAL OR <e>-addrnumber IS INITIAL.
        APPEND VALUE #(
          %key = VALUE #( partner = <e>-partner addrnumber = <e>-addrnumber ) )
          TO failed-addr.
        APPEND VALUE #(
          %msg = new_message_with_text(
                   severity = if_abap_behv_message=>severity-error
                   text     = 'Missing key Partner or Addrnumber.' ) )
          TO reported-addr.
        CONTINUE.
      ENDIF.

      " normalize BP
      lv_bp     = <e>-partner.
      lv_addrno = <e>-addrnumber.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      " block if archived
      CLEAR: ls_cent, lt_msg.
      CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
        EXPORTING
          businesspartner = lv_bp
        IMPORTING
          centraldata     = ls_cent
        TABLES
          return          = lt_msg.
      IF ls_cent-centralarchivingflag = 'X'.
        APPEND VALUE #(
          %key = VALUE #( partner = <e>-partner addrnumber = <e>-addrnumber ) )
          TO failed-addr.
        APPEND VALUE #(
          %msg = new_message_with_text(
                   severity = if_abap_behv_message=>severity-error
                   text     = |BP { lv_bp } is archived. Cannot update address.| ) )
          TO reported-addr.
        CONTINUE.
      ENDIF.

      " map fields & flags
      CLEAR: ls_addr, ls_addrx.

      IF <e>-country IS NOT INITIAL.
        ls_addr-country    = <e>-country.
        ls_addrx-country    = 'X'.
      ENDIF.
      IF <e>-region IS NOT INITIAL.
        ls_addr-region     = <e>-region.
        ls_addrx-region     = 'X'.
      ENDIF.
      IF <e>-postalcode IS NOT INITIAL.
        ls_addr-postl_cod1 = <e>-postalcode.
        ls_addrx-postl_cod1 = 'X'.
      ENDIF.
      IF <e>-street IS NOT INITIAL.
        ls_addr-street     = <e>-street.
        ls_addrx-street     = 'X'.
      ENDIF.
      IF <e>-city IS NOT INITIAL.
        ls_addr-city       = <e>-city.
        ls_addrx-city       = 'X'.
      ENDIF.
      IF <e>-housenumber IS NOT INITIAL.
        ls_addr-house_no   = <e>-housenumber.
        ls_addrx-house_no   = 'X'.
      ENDIF.
      IF <e>-namecompany IS NOT INITIAL.
        ls_addr-c_o_name   = <e>-namecompany. ls_addrx-c_o_name   = 'X'.
      ENDIF.

      " change
      CLEAR lt_msg.
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
        EXPORTING
          businesspartner = lv_bp
          addressdata     = ls_addr
          addressdata_x   = ls_addrx
        TABLES
          return          = lt_msg.

      IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = VALUE #( partner = <e>-partner addrnumber = <e>-addrnumber )
            %msg = new_message(
                     id       = ls_msg-id
                     number   = ls_msg-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_msg-message_v1
                     v2       = ls_msg-message_v2
                     v3       = ls_msg-message_v3
                     v4       = ls_msg-message_v4 ) )
          TO reported-addr.
        ENDLOOP.
        CONTINUE.
      ENDIF.


      APPEND VALUE #(
        %key = VALUE #( partner = <e>-partner addrnumber = <e>-addrnumber )
        %msg = new_message_with_text(
                 severity = if_abap_behv_message=>severity-success
                 text     = |Address { lv_addrno } updated for BP { lv_bp }.| ) )
      TO reported-addr.
    ENDLOOP.
  ENDMETHOD.


  METHOD delete.

    DATA: lv_bp     TYPE bapibus1006_head-bpartner,
          lv_addrno TYPE but020-guid,
          ls_cent   TYPE bapibus1006_central,
          lt_msg    TYPE TABLE OF bapiret2,
          ls_msg    TYPE bapiret2.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).

      IF <k>-partner IS INITIAL OR <k>-addrnumber IS INITIAL.
        APPEND VALUE #( %key = VALUE #( partner = <k>-partner addrnumber = <k>-addrnumber ) )
          TO failed-addr.
        APPEND VALUE #(
          %msg = new_message_with_text(
                   severity = if_abap_behv_message=>severity-error
                   text     = 'Missing key Partner or Addrnumber.' ) )
          TO reported-addr.
        CONTINUE.
      ENDIF.

      lv_bp     = <k>-partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      " block if archived
      CLEAR: ls_cent, lt_msg.
      CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
        EXPORTING
          businesspartner = lv_bp
        IMPORTING
          centraldata     = ls_cent
        TABLES
          return          = lt_msg.
      IF ls_cent-centralarchivingflag = 'X'.
        APPEND VALUE #( %key = VALUE #( partner = <k>-partner addrnumber = <k>-addrnumber ) )
          TO failed-addr.
        APPEND VALUE #(
          %msg = new_message_with_text(
                   severity = if_abap_behv_message=>severity-error
                   text     = |BP { lv_bp } is archived. Cannot remove address.| ) )
          TO reported-addr.
        CONTINUE.
      ENDIF.

      " remove
      lv_addrno = ''.
      CLEAR lt_msg.
      CALL FUNCTION 'BAPI_BUPA_ADDRESS_REMOVE'
        EXPORTING
          businesspartner = lv_bp
          addressguid     = lv_addrno
        TABLES
          return          = lt_msg.

      IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
        LOOP AT lt_msg INTO ls_msg WHERE type = 'E' OR type = 'A'.
          APPEND VALUE #(
            %key = VALUE #( partner = <k>-partner addrnumber = <k>-addrnumber )
            %msg = new_message(
                     id       = ls_msg-id
                     number   = ls_msg-number
                     severity = if_abap_behv_message=>severity-error
                     v1       = ls_msg-message_v1
                     v2       = ls_msg-message_v2
                     v3       = ls_msg-message_v3
                     v4       = ls_msg-message_v4 ) )
          TO reported-addr.
        ENDLOOP.
        CONTINUE.
      ENDIF.

      APPEND VALUE #(
        %key = VALUE #( partner = <k>-partner addrnumber = <k>-addrnumber )
        %msg = new_message_with_text(
                 severity = if_abap_behv_message=>severity-success
                 text     = |Address { lv_addrno } removed from BP { lv_bp }.| ) )
      TO reported-addr.

    ENDLOOP.

  ENDMETHOD.


  METHOD read.

    DATA: lv_bp     TYPE bapibus1006_head-bpartner,
          lv_addrno TYPE ad_addrnum.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<k>).

      IF <k>-partner IS INITIAL OR <k>-addrnumber IS INITIAL.
        CONTINUE.
      ENDIF.

      lv_bp     = <k>-partner.
      lv_addrno = <k>-addrnumber.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      SELECT SINGLE
             b~partner      AS partner,
             b~addrnumber   AS addrnumber,
             a~country      AS country,
             a~region       AS region,
             a~post_code1   AS postalcode,
             a~street       AS street,
             a~city1        AS city,
             a~name_co      AS namecompany,
             a~house_num1   AS housenumber
        FROM but020 AS b
        INNER JOIN adrc  AS a ON a~addrnumber = b~addrnumber
        WHERE b~partner    = @lv_bp
          AND b~addrnumber = @lv_addrno
        INTO @DATA(ls_addr).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " append to result đúng projection Z_C_ADDR
      APPEND VALUE #(
        partner     = ls_addr-partner
        addrnumber  = ls_addr-addrnumber
        country     = ls_addr-country
        region      = ls_addr-region
        postalcode  = ls_addr-postalcode
        street      = ls_addr-street
        city        = ls_addr-city
        namecompany = ls_addr-namecompany
        housenumber = ls_addr-housenumber )
      TO result.

    ENDLOOP.

  ENDMETHOD.


  METHOD rba_root.

    DATA: lv_bp  TYPE bapibus1006_head-bpartner,
          lt_msg TYPE TABLE OF bapiret2,
          ls_det TYPE bapibus1006_central.

    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<k>).

      IF <k>-partner IS INITIAL.
        CONTINUE.
      ENDIF.

      lv_bp = <k>-partner.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_bp
        IMPORTING
          output = lv_bp.

      " lấy central flag để suy ra deletionflag theo BAPI
      CLEAR: ls_det, lt_msg.
      CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
        EXPORTING
          businesspartner = lv_bp
        IMPORTING
          centraldata     = ls_det
        TABLES
          return          = lt_msg.

      " đọc BUT000 để map các trường còn lại
      SELECT SINGLE partner, type, bu_group, bu_sort1, bu_sort2, title,
                     name_org1, name_org2, name_last, name_first, namemiddle,
                     crusr, crdat, crtim, chusr, chdat, chtim
        FROM but000
        WHERE partner = @lv_bp
        INTO @DATA(ls_bu).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      DATA lv_del TYPE c LENGTH 1.
      IF ls_det-centralarchivingflag = 'X'.
        lv_del = 'X'.
      ELSE.
        CLEAR lv_del.
      ENDIF.


      APPEND VALUE #(
        partner      = ls_bu-partner
        type         = ls_bu-type
        bugroup      = ls_bu-bu_group
        busort1      = ls_bu-bu_sort1
        busort2      = ls_bu-bu_sort2
        title        = ls_bu-title
        nameorg1     = ls_bu-name_org1
        nameorg2     = ls_bu-name_org2
        namelast     = ls_bu-name_last
        namefirst    = ls_bu-name_first
        namemiddle   = ls_bu-namemiddle
        crusr        = ls_bu-crusr
        crdat        = ls_bu-crdat
        crtim        = ls_bu-crtim
        chusr        = ls_bu-chusr
        chdat        = ls_bu-chdat
        chtim        = ls_bu-chtim
        deletionflag = lv_del )
      TO result.

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.

CLASS lsc_z_i_root DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_z_i_root IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.
  ENDMETHOD.

  METHOD save.
  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
