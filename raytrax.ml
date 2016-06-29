(* =========================================================================== *
 *
 *    raytrax.ml
 *    ~~~~~~~~~~
 *
 *    A simple raytracer
 *
 *    Project:		  OCamlRaytracer.git
 *
 *    Target language:    OCaml-4.03+flambda
 *
 *    Created 2016-06-28: Ulrich Singer
 *)

module X11 = Graphics


(* The usual suspects. *)

let pi = acos ~-.1.

let epsilon = sqrt epsilon_float

let ( ~@ ) deg = pi *. deg /. 180.


(* Vector representation & arithmetics in 3 dimensions. *)
module V3 =
struct

  type t =
    { x : float;
      y : float;
      z : float; }

  let make x y z =
    { x; y; z; }

  let spread v =
    make v v v

  let add lhs rhs =
    { x = lhs.x +. rhs.x;
      y = lhs.y +. rhs.y;
      z = lhs.z +. rhs.z; }

  let sub lhs rhs =
    { x = lhs.x -. rhs.x;
      y = lhs.y -. rhs.y;
      z = lhs.z -. rhs.z; }

  let mul lhs rhs =
    { x = lhs.x *. rhs.x;
      y = lhs.y *. rhs.y;
      z = lhs.z *. rhs.z; }

  let div lhs rhs =
    { x = lhs.x /. rhs.x;
      y = lhs.y /. rhs.y;
      z = lhs.z /. rhs.z; }

  let smul s vec =
    { x = s *. vec.x;
      y = s *. vec.y;
      z = s *. vec.z; }

  let dot lhs rhs =
       lhs.x *. rhs.x
    +. lhs.y *. rhs.y
    +. lhs.z *. rhs.z

  let cross lhs rhs =
    { x = lhs.y *. rhs.z -. lhs.z *. rhs.y;
      y = lhs.z *. rhs.x -. lhs.x *. rhs.z;
      z = lhs.x *. rhs.y -. lhs.y *. rhs.x; }

  let neg vec =
    { x = -. vec.x;
      y = -. vec.y;
      z = -. vec.z; }

  let abs vec =
    sqrt (dot vec vec)

  let norm vec =
    let r = 1. /. abs vec in
    { x = r *. vec.x;
      y = r *. vec.y;
      z = r *. vec.z; }

  let project a b =
    smul (dot a b /. dot b b) b

  let reflect a b =
    sub a (smul 2. (project a b))

  let zero = spread 0.
  let one  = spread 1.

end (* V3 *)

(* Our 3D vector type. *)
type vec = V3.t

(* A location is a vector from the origin. *)
type loc = vec

(* Vector constructor. *)
let v3 = V3.make


(* Vector operators. *)
let (  +| ) = V3.add
let (  -| ) = V3.sub
let (  *| ) = V3.mul
let (  /| ) = V3.div
let (  %| ) = V3.cross
let (  ~| ) = V3.neg
let ( *.| ) = V3.smul
let ( *|. ) = V3.dot
let ( !|| ) = V3.abs
let ( !|! ) = V3.norm


(* We represent RGB colors as 3D vectors, for access to arithmetics.
   The range for sane component values is [0..1]. *)
module Rgb =
struct

  include V3

  let to_int it =
    let clamp arg = min (max arg 0.) 1. in
    let ir = int_of_float (clamp it.x *. 255.) in
    let ig = int_of_float (clamp it.y *. 255.) in
    let ib = int_of_float (clamp it.z *. 255.) in
    X11.rgb ir ig ib

  let black = V3.zero
  let white = V3.one

end (* Rgb *)

type rgb = Rgb.t

let rgb = Rgb.make


(* Material properties. *)
type mat =
  { (* Material color.  Affects reflected & transmitted light. *)
    color       : Rgb.t;
    (* Diffuse light reflection, [0..1]. *)
    diffuse     : float;
    (* Specular highlight reflection, [0..1]. *)
    specular    : float;
    (* Direct light reflection, [0..1]. *)
    reflection  : float;
    (* Transparency coefficient, [0..1]. *)
    transparcy  : float;
    (* Refraction index, [0..∞). *)
    refractind  : float; }

let mat
  ?(diffuse=1.)
  ?(specular=0.)
  ?(reflection=0.)
  ?(transparcy=0.)
  ?(refractind=0.)
  color
=
  { color; diffuse; specular; reflection; transparcy; refractind; }

(* Refraction index of air at standard temperature and pressure.
   http://hyperphysics.phy-astr.gsu.edu/hbase/tables/indrf.html *)
let _Rix_Air = 1.00029


(* Light & view rays. *)
module Ray =
struct
  type t =
    { origin    : loc;
      direction : vec; }

  let make origin direction =
    { origin; direction; }
end


(* The abstract `body` class. *)
class virtual body material =
object (_ : 'self)
  val material : mat = material
  method material = material
  method virtual normal : loc -> vec
  method virtual scaleby : float -> 'self
  method virtual hitby : Ray.t -> float option
end

(* Convenience cast to `body` for its subclasses. *)
let ( !. ) obj = (obj :> body)

(* A plane of infinite extent. *)
class plane material origin normal =
object
  inherit body material
  val origin : loc = origin
  val normal : vec = V3.norm normal
  method origin = origin
  method normal _ = normal
  method scaleby fac =
    {< origin = fac *.| origin; >}
  method hitby ray =
    let open Ray in
    let ddot = ray.direction *|. normal in
    if ~-.ddot <= epsilon then None
    else
      let dist = (origin -| ray.origin) *|. normal /. ddot in
      if dist > epsilon then Some dist
      else None
end

(* A common sphere. *)
class sphere material center radius =
object
  inherit body material
  val center : loc = center
  val radius : float = radius
  method center = center
  method radius = radius
  method normal at = V3.norm (at -| center)
  method scaleby fac =
    {< center = fac *.| center;
       radius = fac *. radius; >}
  method hitby ray =
    let open Ray in
    let ovec = center -| ray.origin in
    let ddot = ray.direction *|. ovec in
    let radi = radius *. radius -. ovec *|. ovec +. ddot *. ddot in
    if radi < 0. then None
    else
      let root = sqrt radi in
      let dist = ddot -. root in
      if dist > epsilon then Some dist
      else
        let dist = ddot +. root in
        if dist > epsilon then Some dist
        else None
end

(* Light sources. *)
module Lgt =
struct
  type t =
    { center : loc;
      color  : Rgb.t; }

  let make ?(color=Rgb.white) x y z =
    { center = V3.make x y z;
      color; }

  let scale it fac =
    { it with
      center = fac *.| it.center; }
end

type lite = Lgt.t

let lite = Lgt.make


(* A scene defines bodies and lights. *)
type scene =
  { bodies  : body array;
    lights  : lite array;
    (* Ambient lighting coefficient, [0..1]. *)
    ambient : float;
    (* Linear light attenuation coefficient, [0..∞). *)
    linear  : float;
    (* Quadratic light attenuation coefficient, [0..∞). *)
    quadric : float; }


(* Scales a scene by the given factor. *)
let scnscale scn fac =
  { scn with
    bodies  = Array.map (fun bdy -> bdy#scaleby fac) scn.bodies;
    lights  = Array.map (fun lgt -> Lgt.scale lgt fac) scn.lights;
    linear  = scn.linear /. fac;
    quadric = scn.quadric /. fac /. fac; }


(* Computes a light attenuation factor from `scene` properties. *)
let attenuation scn dist =
  1. /. (1. +. scn.linear *. dist +. scn.quadric *. dist *. dist)


(* Computes, from values defined in `scene`, the color of light arriving
   at the origin of the `iray`, where `irix` is the refraction index of
   the medium where the ray origin is located, and `depth` is the allowed
   number of subsequent ray tracing steps. *)
let rec raytint scene iray irix depth =
  let rayfrom loc dir =
    Ray.make (loc +| epsilon *.| dir) dir
  in
  let colortrans rgbvec =
    let lntrans = -.0.15 *.| rgbvec in
    Rgb.(make (exp lntrans.x) (exp lntrans.y) (exp lntrans.z))
  in
  let findhit ?(upto=infinity) bodies ray =
    let curHit = ref (None, upto) in
    for bi = 0 to Array.length bodies - 1 do
      let body = Array.unsafe_get bodies bi in
      let _, dist' = !curHit in
      match body#hitby ray with
      | Some dist when dist < dist' ->
        curHit := (Some body, dist)
      | _ -> ()
    done;
    !curHit
  in
  if depth < 0 then Rgb.black
  else
    match (findhit scene.bodies iray) with
    | (Some body, dist) ->
      (* View ray hit body *)
      let open Ray in
      let matter = body#material in
      let rgbSum = ref (scene.ambient *. matter.diffuse *.| matter.color) in
      let bodypt = iray.origin +| dist *.| iray.direction in
      let bnorm0 = body#normal bodypt in
      let inside = iray.direction *|. bnorm0 > 0. in
      let bnorm = if inside then ~|bnorm0 else bnorm0 in
      let bprix = if inside then matter.refractind else _Rix_Air in
      let attn = attenuation scene dist in
      let opaq = 1. -. matter.transparcy in
      let dyna = 1. -. scene.ambient in
      (* Indirect lighting *)
      if matter.reflection <> 0.
      then begin
        let idir = iray.direction in
        let rdir = idir -| idir *|. bnorm *. 2. *.| bnorm in
        let rray = rayfrom bodypt rdir in
        let tint = raytint scene rray bprix (depth - 1) in
        let fact = dyna *. opaq *. attn *. matter.reflection in
        rgbSum := fact *.| matter.color *| tint +| !rgbSum
      end;
      if matter.refractind <> 0.
      && matter.transparcy <> 0.
      then begin
        let rrel =
          if inside
          then matter.refractind /. irix
          else irix /. matter.refractind
        in
        let trans = matter.color *| colortrans matter.color in
        let fact = dyna *. attn *. matter.transparcy in
        let idir = iray.direction in
        let idot = -.(idir *|. bnorm) in
        let radi = 1. -. rrel *. rrel *. (1. -. idot *. idot) in
        let rdir =
          if radi > 0.
          then  (* Refract *)
            rrel *.| idir +| (rrel *. idot -. sqrt radi) *.| bnorm
          else  (* TIR *)
            ~|(idir -| idir *|. bnorm *. 2. *.| bnorm)
        in
        let rray = rayfrom bodypt rdir in
        let tint = raytint scene rray bprix (depth - 1) in
        rgbSum := fact *.| trans *| tint +| !rgbSum
      end;
      (* Direct lighting *)
      Array.iter
        (fun lgt ->
          let open Lgt in
          let lvec = lgt.center -| bodypt in
          let llen = V3.abs lvec in
          let ldir = 1. /. llen *.| lvec in
          let lray = rayfrom bodypt ldir in
          match (findhit scene.bodies lray ~upto:llen) with
          | (Some bdy, len) ->  (* Closest object blocking the light *)
            let mtr = bdy#material in
            if mtr.transparcy <> 0.
            then begin          (* Add light from transparent object *)
              let ambi = scene.ambient *.| mtr.color in
              let tint = raytint scene lray bprix (depth - 1) -| ambi in
              let attn = attenuation scene len in
              let fact = dyna *. opaq *. attn *. mtr.transparcy in
              rgbSum := fact *.| matter.color *| tint +| !rgbSum;
            end
          | _ ->                (* Nothing between `bodypt` and `lgt` *)
            let attn = attenuation scene llen in
            let ldot = ldir *|. bnorm in
            if matter.diffuse <> 0.
            then begin
              if ldot > 0. then
                let fact = dyna *. opaq *. attn *. ldot *. matter.diffuse in
                let tint = fact *.| matter.color *| lgt.color in
                rgbSum := tint +| !rgbSum
            end;
            if matter.specular <> 0.
            then begin
              let rdir = ldir -| 2. *. ldot *.| bnorm in
              let rdot = iray.direction *|. rdir in
              if rdot > 0. then
                let tint = rdot ** 36. *. matter.specular *.| lgt.color in
                rgbSum := dyna *. opaq *. attn *.| tint +| !rgbSum
            end)
        scene.lights;
      (* Output color *)
      !rgbSum
    | _ ->
      Rgb.black


let render ?(depth=1) scene (xsize,ysize) fov =
  let _ = assert (0. < fov && fov < pi) in
  let _ = assert (xsize > 2) in
  let _ = assert (ysize > 2) in
  let pixls = Array.make_matrix ysize xsize 0 in
  let xhalf = float xsize /. 2. in
  let yhalf = float ysize /. 2. in
  let scale = min xhalf yhalf /. 100. in
  let sscn = scnscale scene scale in
  let dist = max xhalf yhalf /. tan (fov /. 2.) in
  let eye = v3 0. 0. ~-.dist in
  let rpY = ref yhalf in
  for iy = 0 to ysize - 1 do
    let rpX = ref ~-.xhalf in
    let row = Array.unsafe_get pixls iy in
    for ix = 0 to xsize - 1 do
      let loc = v3 !rpX !rpY 0. in
      let dir = V3.norm (loc -| eye) in
      let ray = Ray.make loc dir in
      let pxc = raytint sscn ray _Rix_Air depth |> Rgb.to_int in
      Array.unsafe_set row ix pxc;
      rpX := 1. +. !rpX
    done;
    rpY := !rpY -. 1.
  done;
  pixls


let draw ?depth scene (xsize,ysize) fov =
  let open X11 in
  let pixls = render scene (xsize,ysize) fov ?depth in
  begin try synchronize () with _ -> open_graph "" end;
  remember_mode true;
  display_mode true;
  resize_window xsize ysize;
  let img = make_image pixls in
  draw_image img 0 0


let scn =
  { bodies = [|
      !.(new plane
          (mat (rgb 0.5 1. 0.5) ~diffuse:0.4)
          (v3 0. ~-.100. 0.)
          (v3 0. 1. 0.));
      !.(new sphere
          (mat (rgb 1. 0.5 1.)
            ~diffuse:0.0 ~refractind:1.75 ~transparcy:0.96 ~specular:1.0)
          (v3 0. 0. 200.)
          100.);
      !.(new sphere
          (mat (rgb 0.5 1. 1.) ~diffuse:0.2 ~reflection:0.8 ~specular:0.5)
          (v3 175. 150. 500.)
          50.);
      !.(new sphere
          (mat (rgb 1. 1. 0.5) ~diffuse:0.2 ~reflection:0.8 ~specular:0.5)
          (v3 ~-.150. ~-.75. 350.)
          25.);
      !.(new plane
          (mat (rgb 0.75 0.75 1.) ~diffuse:0. ~reflection:1.)
          (v3 ~-.250. 0. 1000.)
          (v3 0.4 0. ~-.1.0));
      !.(new plane
          (mat (rgb 0.75 0.75 1.) ~diffuse:0. ~reflection:1.)
          (v3 250. 0. 1000.)
          (v3 ~-.0.4 0. ~-.1.0));
      !.(new plane
          (mat (rgb 0.67 0.85 1.) ~diffuse:0.75 ~reflection:1.)
          (v3 0. 0. 0.)
          (v3 0. 0. 1.));
    |];
    lights = [|
      lite 0. 300. 1.;
      lite 0. 600. 450.;
    |];
    ambient = 0.02;
    linear = 0.;
    quadric = 100.e-9;
  }


(* Main entry point. *)
let _ =
  draw scn (1024,640) ~@56. ~depth:7;
  input_char stdin |> ignore


(* ~ raytrax.ml ~ *)
