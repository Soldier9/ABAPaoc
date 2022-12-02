class lcl_202201 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    data: t_elves type standard table of i.
endclass.

class lcl_202201 implementation.
  method lif_solver~part1.
    field-symbols: <lv_line> type string,
                   <lv_elf> type i.

    append initial line to me->t_elves assigning <lv_elf>.
    loop at me->t_input assigning <lv_line>.
      if <lv_line> is initial.
        append initial line to me->t_elves assigning <lv_elf>.
      else.
        <lv_elf> = <lv_elf> + <lv_line>.
      endif.
    endloop.

    sort me->t_elves descending.
    read table me->t_elves index 1 assigning <lv_elf>.
    rv_return = <lv_elf>.
  endmethod.

  method lif_solver~part2.
    field-symbols: <lv_elf> type i.

    loop at me->t_elves from 1 to 3 assigning <lv_elf>.
      rv_return = rv_return + <lv_elf>.
    endloop.
  endmethod.
endclass.


class lcl_202202 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    types: begin of ty_s_points,
             game type string,
             score type i,
           end of ty_s_points,
           ty_t_points type hashed table of ty_s_points with unique key game.
endclass.

class lcl_202202 implementation.
  method lif_solver~part1.
    data: lt_points type ty_t_points,
          ls_points type ty_s_points.

    field-symbols: <ls_line> type string,
                   <ls_points> type ty_s_points.

    ls_points-game = `A X`. ls_points-score = 4. insert ls_points into table lt_points.
    ls_points-game = `A Y`. ls_points-score = 8. insert ls_points into table lt_points.
    ls_points-game = `A Z`. ls_points-score = 3. insert ls_points into table lt_points.
    ls_points-game = `B X`. ls_points-score = 1. insert ls_points into table lt_points.
    ls_points-game = `B Y`. ls_points-score = 5. insert ls_points into table lt_points.
    ls_points-game = `B Z`. ls_points-score = 9. insert ls_points into table lt_points.
    ls_points-game = `C X`. ls_points-score = 7. insert ls_points into table lt_points.
    ls_points-game = `C Y`. ls_points-score = 2. insert ls_points into table lt_points.
    ls_points-game = `C Z`. ls_points-score = 6. insert ls_points into table lt_points.

    loop at me->t_input assigning <ls_line>.
      read table lt_points assigning <ls_points>
                           with key game = <ls_line>.
      rv_return = rv_return + <ls_points>-score.
    endloop.
  endmethod.

  method lif_solver~part2.
    data: lt_points type ty_t_points,
          ls_points type ty_s_points.

    field-symbols: <ls_line> type string,
                   <ls_points> type ty_s_points.

    ls_points-game = `A X`. ls_points-score = 3. insert ls_points into table lt_points.
    ls_points-game = `A Y`. ls_points-score = 4. insert ls_points into table lt_points.
    ls_points-game = `A Z`. ls_points-score = 8. insert ls_points into table lt_points.
    ls_points-game = `B X`. ls_points-score = 1. insert ls_points into table lt_points.
    ls_points-game = `B Y`. ls_points-score = 5. insert ls_points into table lt_points.
    ls_points-game = `B Z`. ls_points-score = 9. insert ls_points into table lt_points.
    ls_points-game = `C X`. ls_points-score = 2. insert ls_points into table lt_points.
    ls_points-game = `C Y`. ls_points-score = 6. insert ls_points into table lt_points.
    ls_points-game = `C Z`. ls_points-score = 7. insert ls_points into table lt_points.

    loop at me->t_input assigning <ls_line>.
      read table lt_points assigning <ls_points>
                           with key game = <ls_line>.
      rv_return = rv_return + <ls_points>-score.
    endloop.
  endmethod.
endclass.
