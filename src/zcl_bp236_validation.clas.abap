CLASS zcl_bp236_validation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      c_action_create TYPE string VALUE 'CREATE',
      c_action_update TYPE string VALUE 'UPDATE',
      c_action_delete TYPE string VALUE 'DELETE'.

    TYPES:
      BEGIN OF ty_bp_check_result,
        exists        TYPE abap_bool,
        is_archived   TYPE abap_bool,
        messages      TYPE bapiret2_t,
        error_message TYPE string,
      END OF ty_bp_check_result.

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
    DATA: ls_cent TYPE bapibus1006_central,
          lt_msg  TYPE bapiret2_t.

    CLEAR: ls_cent, lt_msg.

    CALL FUNCTION 'BAPI_BUPA_CENTRAL_GETDETAIL'
      EXPORTING
        businesspartner = iv_bp
      IMPORTING
        centraldata     = ls_cent
      TABLES
        return          = lt_msg.

    rv_result-messages = lt_msg.

    IF ls_cent-centralarchivingflag = 'X'.
      rv_result-is_archived   = abap_true.

      CASE iv_action.
        WHEN c_action_create.
          rv_result-error_message = |BP { iv_bp } is archived. Cannot create new related data.|.
        WHEN c_action_update.
          rv_result-error_message = |BP { iv_bp } is archived. Cannot update existing record.|.
        WHEN c_action_delete.
          rv_result-error_message = |BP { iv_bp } is archived. Cannot delete record|.
        WHEN OTHERS.
          rv_result-error_message = |BP { iv_bp } is archived. Action not permitted.|.
      ENDCASE.
      rv_result-error_message = |Business Partner { iv_bp } is archived. Cannot update role.|.
    ELSE.
      rv_result-is_archived   = abap_false.
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
        INTO @data(lv_addr)
        FROM but020
        WHERE partner = @iv_bp.
    IF sy-subrc <> 0.
        rv_result-exists = abap_false.
        rv_result-error_message = |Create Address for BP First.|.
    else.
        rv_result-exists = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
