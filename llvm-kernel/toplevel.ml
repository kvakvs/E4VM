(*===----------------------------------------------------------------------===
 * Top-Level parsing and JIT Driver
 *===----------------------------------------------------------------------===*)

open Llvm
open Llvm_executionengine
open Ctypes
open PosixTypes
open Foreign

let name_fun_ugly_hack = ref 0

(* top ::= definition | external | expression | ';' *)
let rec main_loop the_fpm the_execution_engine stream =
  match Stream.peek stream with
  | None -> ()

  (* ignore top-level semicolons. *)
  | Some (Token.Kwd ';') ->
      Stream.junk stream;
      main_loop the_fpm the_execution_engine stream

  | Some token ->
      begin
        try match token with
        | Token.Def ->
            let e = Parser.parse_definition stream in
            print_endline "parsed a function definition.";
            dump_value (Codegen.codegen_func the_fpm e);
        | Token.Extern ->
            let e = Parser.parse_extern stream in
            print_endline "parsed an extern.";
            dump_value (Codegen.codegen_proto e);
        | _ ->
            (* Evaluate a top-level expression into an anonymous function. *)
            let e = Parser.parse_toplevel stream in
            print_endline "parsed a top-level expr";
            let the_function = Codegen.codegen_func the_fpm e in
            (* Problem : Impossible to call several expressions... The
            new function isn't "registered"... *)
            let my_name =
              let v = value_name the_function in
              if v <> "" then (v)
              else
                (
                  let t =  "__my_anom_fct__" ^ (string_of_int !name_fun_ugly_hack) in
                  incr name_fun_ugly_hack;
                  set_value_name t the_function;
                  t
                )
            in
            dump_value the_function;

            (* JIT the function, returning a function pointer. *)
            (* let result = ExecutionEngine.run_function the_function [||]
              the_execution_engine in *)
            let my_name = "my_function_name" in (* If a name has already been add, value_name the_function should contains it. You should be able to use anonyme functions but I don't manage to get them work *)
            let address = get_function_address my_name (funptr (void @-> returning double)) the_execution_engine in
            print_string "Evaluated to ";
            print_float (address ());
            print_newline ();
        with Stream.Error s | Codegen.Error s ->
          (* Skip token for error recovery. *)
          Stream.junk stream;
          print_endline s;
      end;
      print_string "ready> "; flush stdout;
      main_loop the_fpm the_execution_engine stream
