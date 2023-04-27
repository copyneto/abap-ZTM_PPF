class ZCL_IM_COND_PLAN_GREENMILE definition
  public
  final
  create public .

public section.

  interfaces IF_EX_EVAL_SCHEDCOND_PPF .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_COND_PLAN_GREENMILE IMPLEMENTATION.


  method IF_EX_EVAL_SCHEDCOND_PPF~EVALUATE_SCHEDULE_CONDITION.
      DATA: lo_container TYPE REF TO /bofu/cl_ppf_container,
          lt_key       TYPE /bobf/t_frw_key,
          lt_tor_root  TYPE /scmtms/t_tor_root_k.
*          lt_fu        TYPE /scmtms/t_tor_root_k.

    lo_container ?= io_context->appl." io_appl_object.

    APPEND INITIAL LINE TO lt_key ASSIGNING FIELD-SYMBOL(<fs_key>).
    <fs_key>-key = lo_container->get_bo_root_key( ).

    TRY.

        /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key )->retrieve(
          EXPORTING
            iv_node_key             = /scmtms/if_tor_c=>sc_node-root                      " Node
            it_key                  = lt_key                                              " Key Table
          IMPORTING
            et_data                 = lt_tor_root ).

        IF lt_tor_root IS NOT INITIAL.

          SELECT br_notafiscal,
                 freightorder,
                 br_nfedocumentstatus,
                 br_nfiscanceled
            FROM zi_tm_mdf_nf_of
            INTO TABLE @DATA(lt_nf_of)
            FOR ALL ENTRIES IN @lt_tor_root
            WHERE freightorder EQ @lt_tor_root-tor_id. "#EC CI_SEL_DEL

          IF sy-subrc IS INITIAL.

            DELETE lt_nf_of WHERE br_nfedocumentstatus = '01'. "#EC CI_STDSEQ

            IF lt_nf_of IS INITIAL.
              ep_rc = 4.
            ELSE.
              ep_rc = 0.
            ENDIF.
          ELSE.
            ep_rc = 4.
          ENDIF.
        ENDIF.

      CATCH /bobf/cx_frw_contrct_violation. " Caller violates a BOPF contract
        ep_rc = 4.
    ENDTRY.

  endmethod.
ENDCLASS.
