(*

  OCamlEditor
  Copyright (C) 2010, 2011 Francesco Tovagliari

  This file is part of OCamlEditor.

  OCamlEditor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  OCamlEditor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.

*)


let (//) = Filename.concat

let create pixbuf = GMisc.image ~pixbuf ()

let arrow_last_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "arrow_last_14.png")
let arrow_next_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "arrow_next_14.png")
let arrow_prev_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "arrow_prev_14.png")
let attribute = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "attribute.png")
let attribute_mutable = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "attribute_mutable.png")
let attribute_mutable_virtual = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "attribute_mutable_virtual.png")
let attribute_virtual = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "attribute_virtual.png")
let b0 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b0.png")
let b1 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b1.png")
let b2 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b2.png")
let b3 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b3.png")
let b4 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b4.png")
let b5 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b5.png")
let b6 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b6.png")
let b7 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b7.png")
let b8 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b8.png")
let b9 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "b9.png")
let bB = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "bB.png")
let bconf_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "bconf_16.png")
let bconf_24 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "bconf_24.png")
let build_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "build_16.png")
let build_24 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "build_24.png")
let button_close = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "button_close.png")
let button_close_b = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "button_close_b.png")
let button_close_hi = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "button_close_hi.png")
let button_close_hi_b = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "button_close_hi_b.png")
let classe = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "classe.png")
let class_inherit = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "class_inherit.png")
let class_type = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "class_type.png")
let class_virtual = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "class_virtual.png")
let clear_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "clear_16.png")
let clear_24 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "clear_24.png")
let clear_build_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "clear_build_16.png")
let clear_build_24 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "clear_build_24.png")
let compile_all_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "compile_all_16.png")
let compile_file_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "compile_file_16.png")
let dir = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "dir.png")
let empty_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "empty_16.png")
let empty_8 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "empty_8.png")
let error2_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "error2_16.png")
let error_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "error_14.png")
let error_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "error_16.png")
let etask_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "etask_16.png")
let etask_24 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "etask_24.png")
let exc = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "exc.png")
let file_cm = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "file_cm.png")
let file_ml = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "file_ml.png")
let find_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "find_16.png")
let folder_error = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "folder_error.png")
let folder_warning = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "folder_warning.png")
let func = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "func.png")
let lines_in_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "lines_in_14.png")
let lines_out_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "lines_out_14.png")
let load_proj = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "load_proj.png")
let lock_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "lock_14.png")
let met = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "met.png")
let met_private = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "met_private.png")
let met_private_virtual = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "met_private_virtual.png")
let met_virtual = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "met_virtual.png")
let module_impl = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "module_impl.png")
let none_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "none_14.png")
let oe = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "oe.png")
let revert_to_saved_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "revert_to_saved_16.png")
let save_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "save_14.png")
let save_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "save_16.png")
let save_all_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "save_all_16.png")
let save_as_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "save_as_16.png")
let search_again_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "search_again_16.png")
let search_results_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "search_results_16.png")
let simple = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "simple.png")
let square_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "square_14.png")
let start_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "start_16.png")
let typ = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "typ.png")
let type_abstract = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "type_abstract.png")
let type_record = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "type_record.png")
let type_variant = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "type_variant.png")
let warning_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "warning_14.png")
let wrap_lines_16 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "wrap_lines_16.png")
let zoom_in_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "zoom_in_14.png")
let zoom_out_14 = GdkPixbuf.from_file (Oe_config.ocamleditor_pixmaps // "zoom_out_14.png")
