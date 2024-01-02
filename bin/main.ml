open Aoc_2023

let run_test = Array.length Sys.argv > 1 && Sys.argv.(1) = "test"
let () = Day06.solve run_test
