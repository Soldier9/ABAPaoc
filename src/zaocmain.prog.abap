program zaocmain no standard page heading.
*********************************************************************************
* Create solver-classes named lcl_YYYYDD that inherits from lcl_abstract_solver *
* Select inputfile in popup, it needs to be named YYYYDD.txt in order to deduce *
* correct day.                                                                  *
*********************************************************************************

interface lif_solver.
  methods: part1
            returning value(rv_return) type string,
           part2
            returning value(rv_return) type string.
endinterface.

class lcl_abstract_solver definition abstract.
  public section.
    interfaces: lif_solver all methods abstract.

    methods: constructor
              importing it_input type stringtab.

  protected section.
    data: t_input type stringtab.
endclass.

class lcl_abstract_solver implementation.
  method constructor.
    me->t_input = it_input.
  endmethod.
endclass.

include zaoc2021.
include zaoc2022.

form main.
  data: lt_filenames type filetable,
        lv_rc type sysubrc.
  cl_gui_frontend_services=>file_open_dialog( changing file_table = lt_filenames
                                                       rc         = lv_rc ).

  data: lv_filename type string.
  field-symbols: <ls_filename> type file_table.

  read table lt_filenames index 1 assigning <ls_filename> casting.
  assert sy-subrc = 0.
  lv_filename = <ls_filename>-filename.

  data: lt_file type stringtab.
  cl_gui_frontend_services=>gui_upload( exporting filename = lv_filename
                                        changing  data_tab = lt_file ).
  assert sy-subrc = 0.

  " Deduce day from filename
  data: lv_pattern type string value `\\\d{6}\.`,
        lv_offset type i,
        lv_length type i,
        lv_day type string.

  " No proper regex support in this ancient ABAP version, so our match will include \ and . characters
  find regex lv_pattern in lv_filename match offset lv_offset match length lv_length.
  assert sy-subrc = 0.
  lv_offset = lv_offset + 1.
  lv_length = lv_length - 2.
  lv_day = lv_filename+lv_offset(lv_length).

  data: lv_solvername type string,
        lo_solver type ref to lif_solver.
  lv_solvername = |LCL_{ lv_day }|.
  create object lo_solver type (lv_solvername)
   exporting it_input = lt_file.
  assert sy-subrc = 0.

  data: lv_output type string.
  lv_output = |Advent of Code { lv_day+0(4) } Day { lv_day+4(2) }|.
  sy-title = lv_output.
  write: / lv_output.
  uline.

  lv_output = |Part 1 solution: { lo_solver->part1( ) }|.
  write: / lv_output.

  lv_output = |Part 2 solution: { lo_solver->part2( ) }|.
  write: / lv_output.
endform.

initialization.
  perform main.
