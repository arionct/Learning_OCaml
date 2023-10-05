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

let queen8_puzzle_solve () =
  let range = list_make_fwork (fun work -> int1_foreach 8 work) in
  let safe_col = list_make_fwork (fun work -> list_foreach range (fun col -> work (generate_board col, col))) in
  let boards = list_map safe_col (fun (board, col) -> add_queen board col) in
  let final_boards = [] in
  final_boards
