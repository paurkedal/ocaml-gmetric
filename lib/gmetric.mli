(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** The metric type and associated definitions. *)

module Slope : sig
  type t = Zero | Positive | Negative | Both | Unspecified

  val to_int : t -> int
end

module Type : sig

  type _ t =
    | String : string t
    | Uint16 : int t
    | Int16 : int t
    | Uint32 : Int32.t t
    | Int32 : Int32.t t
    | Float : float t
    | Double : float t

  val to_string : 'a t -> string
end

module String_map : Map.S with type key = string

type 'a t = {
  gm_hostname : string option;
  gm_name : string;
  gm_type : 'a Type.t;
  gm_units : string;
  gm_slope : Slope.t;
  gm_tmax : int;
  gm_dmax : int;
  gm_attrs : string String_map.t;
}

val create : ?hostname: string ->
	     ?units: string ->
	     ?tmax: int ->
	     ?dmax: int ->
	     ?slope: Slope.t ->
	     ?group: string ->
	     ?cluster: string ->
	     ?desc: string ->
	     ?title: string ->
	     string -> 'a Type.t -> 'a t
