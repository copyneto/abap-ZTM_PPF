class ZCL_IM_COND_PLAN_GKO definition
  public
  final
  create public .

public section.

  interfaces IF_EX_EVAL_SCHEDCOND_PPF .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_COND_PLAN_GKO IMPLEMENTATION.


  METHOD if_ex_eval_schedcond_ppf~evaluate_schedule_condition.

    DATA : lo_container TYPE REF TO /bofu/cl_ppf_container, "/bofu/cl_ppf_container."
           lt_messages  TYPE /scmtms/t_symsg,
           lr_tor_type  TYPE RANGE OF /scmtms/tor_type,
           lt_key       TYPE /bobf/t_frw_key,
           lt_tor_root  TYPE /scmtms/t_tor_root_k.
*           lt_fu        TYPE /scmtms/t_tor_root_k.

    TRY.
        IF flt_val = 'ZCOND_PLAN_GKO'.
          lo_container ?= io_context->appl." io_appl_object.

          APPEND INITIAL LINE TO lt_key ASSIGNING FIELD-SYMBOL(<fs_key>).
          <fs_key>-key = lo_container->get_bo_root_key( ).

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

              DELETE lt_nf_of WHERE br_nfedocumentstatus = '1'. "#EC CI_STDSEQ

              IF lt_nf_of IS INITIAL.
                ep_rc = 0.
              ELSE.
                ep_rc = 4.
              ENDIF.
            ELSE.
              ep_rc = 4.
            ENDIF.
          ENDIF.


*          /bobf/cl_tra_serv_mgr_factory=>get_service_manager( /scmtms/if_tor_c=>sc_bo_key )->retrieve_by_association(
*            EXPORTING
*              iv_node_key             = /scmtms/if_tor_c=>sc_node-root                     " Node
*              it_key                  = lt_key                                             " Key Table
*              iv_association          = /scmtms/if_tor_c=>sc_association-root-assigned_fus " Association
*              iv_fill_data            = abap_true
*            IMPORTING
*              et_data                 = lt_fu ).
*
*          DELETE lt_fu WHERE dlv_goods_mvmnt = 'C'. "#EC CI_SORTSEQ
*
*          IF lt_fu IS INITIAL.
*            ep_rc = 0.
*          ELSE.
*            ep_rc = 4.
*          ENDIF.

*          DATA(lo_gko) = NEW zcltm_interface_fo_gko( ).
**
*          DATA(lo_config) = NEW zclca_tabela_parametros( ).
*          TRY.
*              lo_config->m_get_range(
*                EXPORTING
*                  iv_modulo = 'TM'
*                  iv_chave1 = 'INTEGRACAO_GKO'
*                  iv_chave2 = 'TOR_TYPE'
*                IMPORTING
*                  et_range  = lr_tor_type ).
*
*            CATCH zcxca_tabela_parametros.
*              ep_rc = 4.
*          ENDTRY.


*          IF lo_gko->check_all_ok(
*            EXPORTING
*              iv_tor_key  = lo_container->get_bo_root_key( )
*            IMPORTING
*              et_messages = lt_messages
*          ) EQ abap_true.
*          ep_rc = 0.
        ELSE.
          ep_rc = 4.
        ENDIF.
      CATCH cx_root.
        ep_rc = 4.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
