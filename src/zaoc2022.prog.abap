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


class lcl_202203 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    methods: get_priority
              importing iv_item type c
              returning value(rv_prio) type i.
endclass.

class lcl_202203 implementation.
  method lif_solver~part1.
    data: lv_compsize type i,
          lv_comp1 type string,
          lv_comp2 type string,
          lv_offset type i,
          lv_item type c.

    field-symbols: <lv_rucksack> type string.

    loop at me->t_input assigning <lv_rucksack>.
      lv_compsize = strlen( <lv_rucksack> ) / 2.
      lv_comp1 = <lv_rucksack>+0(lv_compsize).
      lv_comp2 = <lv_rucksack>+lv_compsize.

      do lv_compsize times.
        lv_offset = sy-index - 1.
        lv_item = lv_comp1+lv_offset(1).
        find lv_item in lv_comp2.
        if sy-subrc = 0.
          rv_return = rv_return + me->get_priority( lv_item ).
          exit.
        endif.
      enddo.
    endloop.
  endmethod.

  method lif_solver~part2.
    data: lv_offset type i,
          lv_item type c.

    field-symbols: <lv_rucksack> type string,
                   <lv_rucksack_pre1> type string,
                   <lv_rucksack_pre2> type string.

    loop at me->t_input assigning <lv_rucksack>.
      if sy-tabix mod 3 = 0.
        read table me->t_input index sy-tabix - 1 assigning <lv_rucksack_pre1>.
        read table me->t_input index sy-tabix - 1 assigning <lv_rucksack_pre2>.

        do strlen( <lv_rucksack> ) times.
          lv_offset = sy-index - 1.
          lv_item = <lv_rucksack>+lv_offset(1).
          find lv_item in <lv_rucksack_pre1>.
          if sy-subrc = 0.
            find lv_item in <lv_rucksack_pre2>.
            if sy-subrc = 0.
              rv_return = rv_return + me->get_priority( lv_item ).
              exit.
            endif.
          endif.
        enddo.
      endif.
    endloop.
  endmethod.

  method get_priority.
    data: lv_offset type i.

    find iv_item in sy-abcde match offset lv_offset.
    if sy-subrc = 0.
      rv_prio = lv_offset + 27.
    else.
      find to_upper( iv_item ) in sy-abcde match offset lv_offset.
      rv_prio = lv_offset + 1.
    endif.
  endmethod.
endclass.


class lcl_202204 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    types: ty_t_intlist type hashed table of i with unique key table_line,
           begin of ty_s_pair,
             elf1 type ty_t_intlist,
             elf2 type ty_t_intlist,
           end of ty_s_pair,
           ty_t_pairs type standard table of ty_s_pair.

    data: t_pairs type ty_t_pairs.

    methods: get_rangeset
              importing iv_range type string
              returning value(rt_set) type ty_t_intlist,
             is_subset
              importing it_set1 type ty_t_intlist
                        it_set2 type ty_t_intlist
              returning value(rv_return) type abap_bool,
             overlaps
              importing it_set1 type ty_t_intlist
                        it_set2 type ty_t_intlist
              returning value(rv_return) type abap_bool.
endclass.

class lcl_202204 implementation.
  method lif_solver~part1.
    data: lv_elf1 type string,
          lv_elf2 type string.

    field-symbols: <lv_line> type string,
                   <ls_pair> type ty_s_pair.

    loop at me->t_input assigning <lv_line>.
      append initial line to me->t_pairs assigning <ls_pair>.
      split <lv_line> at ',' into lv_elf1 lv_elf2.
      <ls_pair>-elf1 = me->get_rangeset( lv_elf1 ).
      <ls_pair>-elf2 = me->get_rangeset( lv_elf2 ).

      if me->is_subset( it_set1 = <ls_pair>-elf1
                        it_set2 = <ls_pair>-elf2 ) = abap_true or
         me->is_subset( it_set1 = <ls_pair>-elf2
                        it_set2 = <ls_pair>-elf1 ) = abap_true.
        rv_return = rv_return + 1.
      endif.
    endloop.
  endmethod.
  method lif_solver~part2.
    field-symbols: <ls_pair> type ty_s_pair.

    loop at me->t_pairs assigning <ls_pair>.
      if me->overlaps( it_set1 = <ls_pair>-elf1
                       it_set2 = <ls_pair>-elf2 ) = abap_true.
        rv_return = rv_return + 1.
      endif.
    endloop.
  endmethod.

  method get_rangeset.
    data: lv_start type string,
          lv_end type string,
          lv_current type i.

    split iv_range at '-' into lv_start lv_end.
    lv_current = lv_start.
    while lv_current <= lv_end.
      insert lv_current into table rt_set.
      lv_current = lv_current + 1.
    endwhile.
  endmethod.

  method is_subset.
    field-symbols: <lv_section> type i.

    loop at it_set1 assigning <lv_section>.
      read table it_set2 transporting no fields
                         with key table_line = <lv_section>.
      if sy-subrc > 0.
        rv_return = abap_false.
        return.
      endif.
    endloop.
    rv_return = abap_true.
  endmethod.

  method overlaps.
    field-symbols: <lv_section> type i.

    loop at it_set1 assigning <lv_section>.
      read table it_set2 transporting no fields
                         with key table_line = <lv_section>.
      if sy-subrc = 0.
        rv_return = abap_true.
        return.
      endif.
    endloop.
  endmethod.
endclass.


class lcl_202205 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    types: begin of ty_s_stack,
             num type i,
             stack type string,
           end of ty_s_stack,
           ty_t_stacks type hashed table of ty_s_stack with unique key num.
endclass.

class lcl_202205 implementation.
  method lif_solver~part1.
    data: lt_stacks type ty_t_stacks,
          lv_parsing_stacks type abap_bool value abap_true,
          lt_matches type match_result_tab,
          lv_stacknum type i,
          ls_stack type ty_s_stack,
          lv_moves type i,
          lv_offset type i.

    field-symbols: <lv_line> type string,
                   <ls_match> type match_result,
                   <ls_stack> type ty_s_stack,
                   <ls_stackfrom> type ty_s_stack.

    loop at me->t_input assigning <lv_line>.
      if lv_parsing_stacks = abap_true.
        lt_matches = cl_abap_matcher=>create( pattern = `\[\w\]`
                                              text    = <lv_line> )->find_all( ).
        loop at lt_matches assigning <ls_match>.
          lv_stacknum = ( <ls_match>-offset / 4 ) + 1.
          read table lt_stacks assigning <ls_stack>
                               with key num = lv_stacknum.
          if sy-subrc > 0.
            ls_stack-num = lv_stacknum.
            insert ls_stack into table lt_stacks assigning <ls_stack>.
          endif.
          <ls_match>-offset = <ls_match>-offset + 1.
          <ls_stack>-stack = <ls_stack>-stack && <lv_line>+<ls_match>-offset(1).
        endloop.
        if <lv_line> is initial.
          lv_parsing_stacks = abap_false.
          loop at lt_stacks assigning <ls_stack>.
            <ls_stack>-stack = reverse( <ls_stack>-stack ).
          endloop.
        endif.
      else.
        lt_matches = cl_abap_matcher=>create( pattern = `\d+`
                                              text    = <lv_line> )->find_all( ).
        read table lt_matches index 1 assigning <ls_match>.
        lv_moves = <lv_line>+<ls_match>-offset(<ls_match>-length).

        read table lt_matches index 2 assigning <ls_match>.
        lv_stacknum = <lv_line>+<ls_match>-offset(<ls_match>-length).
        read table lt_stacks assigning <ls_stackfrom>
                             with key num = lv_stacknum.

        read table lt_matches index 3 assigning <ls_match>.
        lv_stacknum = <lv_line>+<ls_match>-offset(<ls_match>-length).
        read table lt_stacks assigning <ls_stack>
                             with key num = lv_stacknum.

        do lv_moves times.
          lv_offset = strlen( <ls_stackfrom>-stack ) - 1.
          <ls_stack>-stack = <ls_stack>-stack && <ls_stackfrom>-stack+lv_offset(1).
          <ls_stackfrom>-stack = <ls_stackfrom>-stack+0(lv_offset).
        enddo.
      endif.
    endloop.

    do lines( lt_stacks ) times.
      read table lt_stacks assigning <ls_stack>
                           with key num = sy-index.
      lv_offset = strlen( <ls_stack>-stack ) - 1.
      rv_return = rv_return && <ls_stack>-stack+lv_offset(1).
    enddo.
  endmethod.

  method lif_solver~part2.
    data: lt_stacks type ty_t_stacks,
          lv_parsing_stacks type abap_bool value abap_true,
          lt_matches type match_result_tab,
          lv_stacknum type i,
          ls_stack type ty_s_stack,
          lv_moves type i,
          lv_offset type i.

    field-symbols: <lv_line> type string,
                   <ls_match> type match_result,
                   <ls_stack> type ty_s_stack,
                   <ls_stackfrom> type ty_s_stack.

    loop at me->t_input assigning <lv_line>.
      if lv_parsing_stacks = abap_true.
        lt_matches = cl_abap_matcher=>create( pattern = `\[\w\]`
                                              text    = <lv_line> )->find_all( ).
        loop at lt_matches assigning <ls_match>.
          lv_stacknum = ( <ls_match>-offset / 4 ) + 1.
          read table lt_stacks assigning <ls_stack>
                               with key num = lv_stacknum.
          if sy-subrc > 0.
            ls_stack-num = lv_stacknum.
            insert ls_stack into table lt_stacks assigning <ls_stack>.
          endif.
          <ls_match>-offset = <ls_match>-offset + 1.
          <ls_stack>-stack = <ls_stack>-stack && <lv_line>+<ls_match>-offset(1).
        endloop.
        if <lv_line> is initial.
          lv_parsing_stacks = abap_false.
          loop at lt_stacks assigning <ls_stack>.
            <ls_stack>-stack = reverse( <ls_stack>-stack ).
          endloop.
        endif.
      else.
        lt_matches = cl_abap_matcher=>create( pattern = `\d+`
                                              text    = <lv_line> )->find_all( ).
        read table lt_matches index 1 assigning <ls_match>.
        lv_moves = <lv_line>+<ls_match>-offset(<ls_match>-length).

        read table lt_matches index 2 assigning <ls_match>.
        lv_stacknum = <lv_line>+<ls_match>-offset(<ls_match>-length).
        read table lt_stacks assigning <ls_stackfrom>
                             with key num = lv_stacknum.

        read table lt_matches index 3 assigning <ls_match>.
        lv_stacknum = <lv_line>+<ls_match>-offset(<ls_match>-length).
        read table lt_stacks assigning <ls_stack>
                             with key num = lv_stacknum.

        lv_offset = strlen( <ls_stackfrom>-stack ) - lv_moves.
        <ls_stack>-stack = <ls_stack>-stack && <ls_stackfrom>-stack+lv_offset(lv_moves).
        <ls_stackfrom>-stack = <ls_stackfrom>-stack+0(lv_offset).
      endif.
    endloop.

    do lines( lt_stacks ) times.
      read table lt_stacks assigning <ls_stack>
                           with key num = sy-index.
      lv_offset = strlen( <ls_stack>-stack ) - 1.
      rv_return = rv_return && <ls_stack>-stack+lv_offset(1).
    enddo.
  endmethod.
endclass.


class lcl_202206 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    methods: finduniquesequence
              importing iv_string type string
                        iv_length type i
              returning value(rv_endofseq) type i.
endclass.


class lcl_202206 implementation.
  method lif_solver~part1.
    field-symbols: <lv_line> type string.

    read table me->t_input index 1 assigning <lv_line>.
    rv_return = me->finduniquesequence( iv_string = <lv_line>
                                        iv_length = 4 ).
  endmethod.

  method lif_solver~part2.
    field-symbols: <lv_line> type string.

    read table me->t_input index 1 assigning <lv_line>.
    rv_return = me->finduniquesequence( iv_string = <lv_line>
                                        iv_length = 14 ).
  endmethod.

  method finduniquesequence.
    data: lt_chars type hashed table of string with unique key table_line,
          lv_offset1 type i,
          lv_offset2 type i.

    lv_offset1 = iv_length - 1.
    while lv_offset1 < strlen( iv_string ).
      clear lt_chars.
      lv_offset2 = lv_offset1 - ( iv_length - 1 ).
      while lv_offset2 <= lv_offset1.
        insert iv_string+lv_offset2(1) into table lt_chars.
        lv_offset2 = lv_offset2 + 1.
      endwhile.
      if lines( lt_chars ) = iv_length.
        rv_endofseq = lv_offset1 + 1.
        return.
      endif.
      lv_offset1 = lv_offset1 + 1.
    endwhile.
  endmethod.
endclass.


class lcl_efsFile definition final create private.
  public section.
    data: name type string.

    class-methods: create_instance
                    importing iv_name type string
                              iv_size type i
                    returning value(ro_instance) type ref to lcl_efsFile.

    methods: size
              returning value(rv_size) type i.

  private section.
    data: _size type i.

    methods: constructor
              importing iv_name type string
                        iv_size type i.
endclass.

class lcl_efsFile implementation.
  method create_instance.
    create object ro_instance
     exporting iv_name = iv_name
               iv_size = iv_size.
  endmethod.

  method constructor.
    me->name = iv_name.
    me->_size = iv_size.
  endmethod.

  method size.
    rv_size = me->_size.
  endmethod.
endclass.

class lcl_efsDir definition final create private.
  public section.
    types: begin of ty_s_efsEntry,
             dir type ref to lcl_efsDir,
             file type ref to lcl_efsFile,
           end of ty_s_efsEntry.

    data: name type string,
          o_parent type ref to lcl_efsDir,
          t_content type standard table of ty_s_efsEntry.

    class-methods: create_instance
                    importing iv_name type string
                              iv_parent type ref to lcl_efsDir optional
                    returning value(ro_instance) type ref to lcl_efsDir.

    methods: size
              returning value(rv_size) type i,
             sumDirsUpTo
              importing iv_limit type i
              returning value(rv_size) type i,
             smallestAtLeast
              importing iv_limit type i
              returning value(ro_result) type ref to lcl_efsDir.

  private section.
    methods: constructor
              importing iv_name type string
                        iv_parent type ref to lcl_efsDir.
endclass.

class lcl_efsDir implementation.
  method create_instance.
    create object ro_instance
     exporting iv_name = iv_name
               iv_parent = iv_parent.
  endmethod.

  method constructor.
    me->name = iv_name.
    if iv_parent is not initial.
      me->o_parent = iv_parent.
    endif.
  endmethod.

  method size.
    field-symbols: <ls_efsentry> type ty_s_efsentry.
    loop at me->t_content assigning <ls_efsentry>.
      if <ls_efsentry>-dir is not initial.
        rv_size = rv_size + <ls_efsentry>-dir->size( ).
      else.
        rv_size = rv_size + <ls_efsentry>-file->size( ).
      endif.
    endloop.
  endmethod.

  method sumDirsUpTo.
    data: lv_size type i,
          lo_tmpDir type ref to lcl_efsDir.

    field-symbols: <ls_efsentry> type ty_s_efsEntry.

    lv_size = me->size( ).
    if lv_size <= iv_limit.
      rv_size = rv_size + lv_size.
    endif.

    loop at me->t_content assigning <ls_efsentry>.
      if <ls_efsentry>-dir is not initial.
        rv_size = rv_size + <ls_efsentry>-dir->sumDirsUpTo( iv_limit ).
      endif.
    endloop.
  endmethod.

  method smallestAtLeast.
    data: lv_size type i,
          lo_other type ref to lcl_efsdir.

    field-symbols: <ls_efsentry> type ty_s_efsentry.

    loop at me->t_content assigning <ls_efsentry>
                          where dir is not initial.
      lv_size = <ls_efsentry>-dir->size( ).
      if lv_size >= iv_limit and ( ro_result is initial or ro_result->size( ) > lv_size ).
        ro_result = <ls_efsentry>-dir.
      endif.

      lo_other = <ls_efsentry>-dir->smallestAtLeast( iv_limit ).
      if lo_other is not initial and ( ro_result is initial or ro_result->size( ) > lo_other->size( ) ).
        ro_result = lo_other.
      endif.
    endloop.
  endmethod.
endclass.

class lcl_202207 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    data: o_efs type ref to lcl_efsdir.
endclass.


class lcl_202207 implementation.
  method lif_solver~part1.
    data: lv_part1 type string,
          lv_part2 type string,
          lo_tmpDir type ref to lcl_efsDir,
          lo_tmpFile type ref to lcl_efsFile,
          lv_size type i.

    field-symbols: <lv_line> type string,
                   <lo_pwd> type ref to lcl_efsDir,
                   <ls_efsentry> type lcl_efsDir=>ty_s_efsEntry.

    me->o_efs = lcl_efsdir=>create_instance( `/` ).

    loop at me->t_input assigning <lv_line>.
      if <lv_line>+0(1) = `$`.
        split <lv_line>+2 at ` ` into lv_part1 lv_part2.
        if lv_part1 = `cd`.
          if lv_part2 = `/`. assign me->o_efs to <lo_pwd>.
          elseif lv_part2 = `..`. assign <lo_pwd>->o_parent to <lo_pwd>.
          else.
            loop at <lo_pwd>->t_content assigning <ls_efsentry>.
              if <ls_efsentry>-dir is not initial and <ls_efsentry>-dir->name = lv_part2.
                assign <ls_efsentry>-dir to <lo_pwd>.
                exit.
              endif.
            endloop.
          endif.
        endif.
      else.
        split <lv_line> at ` ` into lv_part1 lv_part2.
        append initial line to <lo_pwd>->t_content assigning <ls_efsentry>.
        if lv_part1 = `dir`.
          <ls_efsentry>-dir = lcl_efsDir=>create_instance( iv_name   = lv_part2
                                                           iv_parent = <lo_pwd> ).
        else.
          lv_size = lv_part1.
          <ls_efsentry>-file = lcl_efsFile=>create_instance( iv_name = lv_part2
                                                             iv_size = lv_size ).
        endif.
      endif.
    endloop.

    rv_return = me->o_efs->sumDirsUpTo( 100000 ).
  endmethod.

  method lif_solver~part2.
    data: lv_freespace type i,
          lv_neededspace type i.

    lv_freespace = 70000000 - me->o_efs->size( ).
    lv_neededspace = 30000000 - lv_freespace.

    rv_return = me->o_efs->smallestAtLeast( lv_neededspace )->size( ).
  endmethod.
endclass.


class lcl_202208 definition final inheriting from lcl_abstract_solver.
  public section.
    methods: lif_solver~part1 redefinition,
             lif_solver~part2 redefinition.

  private section.
    data: width type i,
          height type i.
endclass.


class lcl_202208 implementation.
  method lif_solver~part1.
    data: lv_x type i,
          lv_y type i,
          lv_x2 type i,
          lv_y2 type i,
          lv_visible type i,
          lv_isvisible type abap_bool.

    field-symbols: <lv_line> type string,
                   <lv_line2> type string.

    read table me->t_input index 1 assigning <lv_line>.
    me->height = lines( me->t_input ).
    me->width = strlen( <lv_line> ).

    lv_y = 1.
    while lv_y <= me->height.
      read table me->t_input index lv_y assigning <lv_line>.
      lv_x = -1.
      while lv_x < me->width - 1.
        lv_x = lv_x + 1. " Increment at the start of loop because of the use of continue!
        if lv_x = 0 or lv_y = 1 or lv_x = me->width - 1 or lv_y = me->height.
          lv_visible = lv_visible + 1.
        else.
          lv_isvisible = abap_true.
          lv_x2 = lv_x - 1.
          while lv_x2 >= 0.
            if <lv_line>+lv_x2(1) >= <lv_line>+lv_x(1). lv_isvisible = abap_false. exit. endif.
            lv_x2 = lv_x2 - 1.
          endwhile.
          if lv_isvisible = abap_true. lv_visible = lv_visible + 1. continue. endif.

          lv_isvisible = abap_true.
          lv_x2 = lv_x + 1.
          while lv_x2 < me->width.
            if <lv_line>+lv_x2(1) >= <lv_line>+lv_x(1). lv_isvisible = abap_false. exit. endif.
            lv_x2 = lv_x2 + 1.
          endwhile.
          if lv_isvisible = abap_true. lv_visible = lv_visible + 1. continue. endif.

          lv_isvisible = abap_true.
          lv_y2 = lv_y - 1.
          while lv_y2 > 0.
            read table me->t_input index lv_y2 assigning <lv_line2>.
            if <lv_line2>+lv_x(1) >= <lv_line>+lv_x(1). lv_isvisible = abap_false. exit. endif.
            lv_y2 = lv_y2 - 1.
          endwhile.
          if lv_isvisible = abap_true. lv_visible = lv_visible + 1. continue. endif.

          lv_isvisible = abap_true.
          lv_y2 = lv_y + 1.
          while lv_y2 <= me->height.
            read table me->t_input index lv_y2 assigning <lv_line2>.
            if <lv_line2>+lv_x(1) >= <lv_line>+lv_x(1). lv_isvisible = abap_false. exit. endif.
            lv_y2 = lv_y2 + 1.
          endwhile.
          if lv_isvisible = abap_true. lv_visible = lv_visible + 1. endif.
        endif.
      endwhile.
      lv_y = lv_y + 1.
    endwhile.

    rv_return = lv_visible.
  endmethod.
  method lif_solver~part2.
    data: lv_x type i,
          lv_y type i,
          lv_x2 type i,
          lv_y2 type i,
          lv_bestscore type i,
          lv_treescore type i,
          lv_dirscore type i.

    field-symbols: <lv_line> type string,
                   <lv_line2> type string.
    lv_y = 1.
    while lv_y <= me->height.
      read table me->t_input index lv_y assigning <lv_line>.
      lv_x = -1.
      while lv_x < me->width - 1.
        lv_x = lv_x + 1. " Increment at the start of loop because of the use of continue!

        lv_treescore = 1.
        lv_dirscore = 0.
        lv_x2 = lv_x - 1.
        while lv_x2 >= 0.
          lv_dirscore = lv_dirscore + 1.
          if <lv_line>+lv_x2(1) >= <lv_line>+lv_x(1). exit. endif.
          lv_x2 = lv_x2 - 1.
        endwhile.
        lv_treescore = lv_treescore * lv_dirscore.

        lv_dirscore = 0.
        lv_x2 = lv_x + 1.
        while lv_x2 < me->width.
          lv_dirscore = lv_dirscore + 1.
          if <lv_line>+lv_x2(1) >= <lv_line>+lv_x(1). exit. endif.
          lv_x2 = lv_x2 + 1.
        endwhile.
        lv_treescore = lv_treescore * lv_dirscore.

        lv_dirscore = 0.
        lv_y2 = lv_y - 1.
        while lv_y2 > 0.
          lv_dirscore = lv_dirscore + 1.
          read table me->t_input index lv_y2 assigning <lv_line2>.
          if <lv_line2>+lv_x(1) >= <lv_line>+lv_x(1). exit. endif.
          lv_y2 = lv_y2 - 1.
        endwhile.
        lv_treescore = lv_treescore * lv_dirscore.

        lv_dirscore = 0.
        lv_y2 = lv_y + 1.
        while lv_y2 <= me->height.
          lv_dirscore = lv_dirscore + 1.
          read table me->t_input index lv_y2 assigning <lv_line2>.
          if <lv_line2>+lv_x(1) >= <lv_line>+lv_x(1). exit. endif.
          lv_y2 = lv_y2 + 1.
        endwhile.
        lv_treescore = lv_treescore * lv_dirscore.

        lv_bestscore = nmax( val1 = lv_bestscore
                             val2 = lv_treescore ).
      endwhile.
      lv_y = lv_y + 1.
    endwhile.

    rv_return = lv_bestscore.
  endmethod.
endclass.
