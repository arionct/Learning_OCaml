#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type board_t = int * int * int * int * int * int * int * int

let list_map = fun xs -> foreach_to_map_list(list_foreach)(xs)

let safe (board: board_t) col row =
  let (_, b1, b2, b3, b4, b5, b6, b7) = board in
  let not_on_same_col = not (col = b1 || col = b2 || col = b3 || col = b4 || col = b5 || col = b6 || col = b7) in
  let not_on_same_diag1 = not (col - b1 = row - 1 || col - b2 = row - 2 || col - b3 = row - 3 || col - b4 = row - 4 || col - b5 = row - 5 || col - b6 = row - 6 || col - b7 = row - 7) in
  let not_on_same_diag2 = not (col - b1 = -row + 1 || col - b2 = -row + 2 || col - b3 = -row + 3 || col - b4 = -row + 4 || col - b5 = -row + 5 || col - b6 = -row + 6 || col - b7 = -row + 7) in
  not_on_same_col && not_on_same_diag1 && not_on_same_diag2

let add_queen (board: board_t) col =
  let (b0, b1, b2, b3, b4, b5, b6, b7) = board in
  (col, b0, b1, b2, b3, b4, b5, b6)

let generate_board col = (col, 0, 0, 0, 0, 0, 0, 0)

let queen8_puzzle_solve (): board_t list =
  let rec range start stop =
    if start > stop then []
    else start :: range (start + 1) stop
  in
  
  let safe_rows = range 1 8 in
  
  let safe_cols = range 1 8 in

  let generate_board row col =
    (row, col, 0, 0, 0, 0, 0, 0)
  in

  let add_queen (board, col) =
    let try_row row =
      let result = ref [] in
      let r = ref row in
      while !r <= 8 do
        if not (!r = fst board || col = snd board || abs (!r - fst board) = abs (col - snd board)) then
          result := (!r, col) :: !result;
        r := !r + 1
      done;
      !result
    in
    try_row 1
  in

  let safe_col = foreach_to_listize (list_foreach safe_cols) (fun col -> (generate_board 1 col, col)) in

  let boards = list_map safe_col (fun (board, col) -> add_queen board col) in

  let final_boards = list_make_filter (fun board -> string_length (string_filter (string_of_int (fst board)) (fun c -> c = ' ')) = 8) (fun (_, board) -> list_make_fwork (fun work -> string_foreach (string_of_int board) work)) in

  list_map final_boards (fun board -> board)
