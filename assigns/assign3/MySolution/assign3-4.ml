#use "./../../../classlib/OCaml/MyOCaml.ml";;

let list_of_buddies(word: string): string list =
  let n = String.length word in
  let buddies = ref [] in
  for i = 0 to n - 1 do
    for c = 'a' to 'z' do
      if c <> word.[i] then begin
        let buddy = String.copy word in
        buddy.[i] <- c;
        buddies := buddy :: !buddies;
      end
    done;
  done;
  !buddies
;;