class virtual viewer =
  object
    method virtual display : filename:string -> unit
    method virtual coerce : GObj.widget
    method virtual destroy : unit -> unit
  end

module type DEVICE = sig
  val lang : string
  val have_embedded_viewer : bool
  val create : ?packing:(GObj.widget -> unit) -> unit -> viewer option
  val draw : filename:string -> viewer option -> unit
end


let device : (module DEVICE) ref = ref (module Dot_viewer_pdf.PDF : DEVICE)

(*let rec get_device () =
  match !device with
    | Some dev -> dev
    | None ->
      if not (Plugin.load "dot_viewer_svg.cma") then
        (device := Some (module Dot_viewer_pdf.PDF : DEVICE));
      get_device()*)


