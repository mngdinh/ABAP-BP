CLASS zcl_bp236_validation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      c_action_create TYPE string VALUE 'CREATE',
      c_action_update TYPE string VALUE 'UPDATE',
      c_action_delete TYPE string VALUE 'DELETE',
      c_action_read   TYPE string VALUE 'READ',
      c_bp_person     TYPE but000-type VALUE '1',
      c_bp_org        TYPE but000-type VALUE '2'.


    TYPES:
      BEGIN OF ty_bp_check_result,
        exists        TYPE abap_bool,
        is_archived   TYPE abap_bool,
        is_fault      TYPE abap_bool,
        messages      TYPE bapiret2_t,
        error_message TYPE string,
      END OF ty_bp_check_result.

    "validate create bp
    CLASS-METHODS validate_create_bp
      IMPORTING !e               TYPE z_i_root
      RETURNING VALUE(rv_result) TYPE ty_bp_check_result.

    "Check archived status
    CLASS-METHODS check_bp_not_archived
      IMPORTING
        !iv_bp           TYPE bu_partner
        !iv_action       TYPE string OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE ty_bp_check_result.

    "Check exist BP
    CLASS-METHODS check_bp_existence
      IMPORTING
        !iv_bp           TYPE bu_partner
      RETURNING
        VALUE(rv_result) TYPE ty_bp_check_result.

    CLASS-METHODS check_bp_address
      IMPORTING
        !iv_bp           TYPE bu_partner
      RETURNING
        VALUE(rv_result) TYPE ty_bp_check_result.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bp236_validation IMPLEMENTATION.


  METHOD check_bp_not_archived.
    DATA: lv_bp    TYPE bu_partner,
          ls_cent  TYPE bapibus1006_central,
          lt_msg   TYPE bapiret2_t,
          lv_xdele TYPE but000-xdele.

    " Chuẩn hoá key
    lv_bp = iv_bp.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_bp
      IMPORTING
        output = lv_bp.

    CLEAR: ls_cent, lt_msg, lv_xdele, rv_result.

    " 1) Tồn tại?
    SELECT SINGLE xdele
      FROM but000
      WHERE partner = @lv_bp
      INTO @lv_xdele.
    IF sy-subrc <> 0.
      rv_result-exists        = abap_false.
      rv_result-is_archived   = abap_false.
      rv_result-error_message = |Business Partner { lv_bp } does not exist.|.
      RETURN.
    ENDIF.

    rv_result-exists = abap_true.
    IF lv_xdele = 'X'.
      rv_result-is_archived = abap_true.
      CASE iv_action.
        WHEN c_action_create.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Cannot create related data.|.
        WHEN c_action_update.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Cannot update.|.
        WHEN c_action_delete.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Cannot delete.|.
        WHEN c_action_read.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Read not permitted.|.
        WHEN OTHERS.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Action not permitted.|.
      ENDCASE.
      RETURN.
    ENDIF.

    " 3) Chuẩn hoá theo BAPI flag
    CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
      EXPORTING
        businesspartner = lv_bp
      IMPORTING
        centraldata     = ls_cent
      TABLES
        return          = lt_msg.

    rv_result-messages = lt_msg.

    IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
      rv_result-exists        = abap_false.
      rv_result-is_archived   = abap_false.
      rv_result-error_message = |Business Partner { lv_bp } does not exist.|.
      RETURN.
    ENDIF.

    IF ls_cent-centralarchivingflag = 'X'.
      rv_result-is_archived = abap_true.
      CASE iv_action.
        WHEN c_action_create.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Cannot create related data.|.
        WHEN c_action_update.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Cannot update.|.
        WHEN c_action_delete.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Cannot delete.|.
        WHEN c_action_read.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Read not permitted.|.
        WHEN OTHERS.
          rv_result-error_message = |Business Partner { lv_bp } is archived. Action not permitted.|.
      ENDCASE.
    ELSE.
      rv_result-is_archived   = abap_false.
      CLEAR rv_result-error_message.
    ENDIF.
  ENDMETHOD.


  METHOD check_bp_existence.
    DATA: lt_msg TYPE bapiret2_t.

    CLEAR lt_msg.

    CALL FUNCTION 'BAPI_BUPA_EXISTENCE_CHECK'
      EXPORTING
        businesspartner = iv_bp
      TABLES
        return          = lt_msg.

    rv_result-messages = lt_msg.

    IF line_exists( lt_msg[ type = 'E' ] ) OR line_exists( lt_msg[ type = 'A' ] ).
      rv_result-exists        = abap_false.
      rv_result-error_message = |Business Partner { iv_bp } does not exist.|.
    ELSE.
      rv_result-exists = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_bp_address.
    SELECT SINGLE addrnumber
        INTO @DATA(lv_addr)
        FROM but020
        WHERE partner = @iv_bp.
    IF sy-subrc <> 0.
      rv_result-exists = abap_false.
      rv_result-error_message = |Create Address for BP First.|.
    ELSE.
      rv_result-exists = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD validate_create_bp.
    rv_result-is_fault = abap_false.

    "validate bp type
    IF e-type IS NOT INITIAL.
      CASE e-type.
        WHEN c_bp_person.
          IF e-namefirst IS INITIAL OR
             e-namelast IS INITIAL.
            rv_result-error_message = |First Name or Last Name is not null for type 'Person'|.
            rv_result-is_fault = abap_true.
            EXIT.
          ENDIF.
        WHEN c_bp_org.
          IF e-nameorg1 IS INITIAL.
            rv_result-error_message = |Organization is not null for type 'Org'|.
            rv_result-is_fault = abap_true.
            EXIT.
          ENDIF.
        WHEN OTHERS.
          rv_result-error_message = |Wrong type.|.
          rv_result-is_fault = abap_true.
          EXIT.
      ENDCASE.
    ELSE.
      rv_result-error_message = |Type is required|.
      rv_result-is_fault = abap_true.
      EXIT.
    ENDIF.

    "validate bugroup
    IF e-bugroup IS NOT INITIAL.
      "check bugroup is exist or not
      SELECT bu_group FROM tb001 INTO @DATA(lv_bugr)
        WHERE bu_group = @e-bugroup.
      ENDSELECT.
      IF sy-subrc <> 0.
        rv_result-error_message = |Grouping is not exist|.
        rv_result-is_fault = abap_true.
        EXIT.
      ENDIF.
    ELSE.
      rv_result-error_message = |Grouping is required|.
      rv_result-is_fault = abap_true.
      EXIT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
