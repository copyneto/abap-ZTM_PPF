class ZCL_IM_COND_EXEC_TRAFEGUS definition
  public
  final
  create public .

public section.

  interfaces IF_EX_EVAL_STARTCOND_PPF .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_COND_EXEC_TRAFEGUS IMPLEMENTATION.


  method IF_EX_EVAL_STARTCOND_PPF~EVALUATE_START_CONDITION.
    "Validar.
*    ep_rc = 4.

    CHECK sy-uname EQ 'S-PIUSER'.


  endmethod.
ENDCLASS.
