(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Deterministic, runtime-materialized CBL-08 media/schema fixtures. *)

open Cabal

type t = {
  attachment : Backend_types.media_attachment;
  bytes : string;
  response_field : string;
  expected_value : string;
}

type response_error = Schema_rejected | Image_semantics_mismatch

let uint32_be value =
  let bytes = Bytes.create 4 in
  Bytes.set bytes 0
    (Char.chr Int32.(to_int (logand (shift_right_logical value 24) 0xffl))) ;
  Bytes.set bytes 1
    (Char.chr Int32.(to_int (logand (shift_right_logical value 16) 0xffl))) ;
  Bytes.set bytes 2
    (Char.chr Int32.(to_int (logand (shift_right_logical value 8) 0xffl))) ;
  Bytes.set bytes 3 (Char.chr Int32.(to_int (logand value 0xffl))) ;
  Bytes.unsafe_to_string bytes

let uint16_le value =
  String.init 2 (function
    | 0 -> Char.chr (value land 0xff)
    | _ -> Char.chr ((value lsr 8) land 0xff))

let crc32 value =
  let crc = ref Int32.minus_one in
  String.iter
    (fun character ->
      crc := Int32.logxor !crc (Int32.of_int (Char.code character)) ;
      for _ = 0 to 7 do
        crc :=
          if Int32.logand !crc 1l <> 0l then
            Int32.logxor
              (Int32.shift_right_logical !crc 1)
              0xedb88320l
          else Int32.shift_right_logical !crc 1
      done)
    value ;
  Int32.lognot !crc

let adler32 value =
  let modulo = 65_521 in
  let a = ref 1 in
  let b = ref 0 in
  String.iter
    (fun character ->
      a := (!a + Char.code character) mod modulo ;
      b := (!b + !a) mod modulo)
    value ;
  Int32.logor
    (Int32.shift_left (Int32.of_int !b) 16)
    (Int32.of_int !a)

let png_chunk kind payload =
  let body = kind ^ payload in
  uint32_be (Int32.of_int (String.length payload)) ^ body
  ^ uint32_be (crc32 body)

let stored_zlib value =
  let length = String.length value in
  if length > 0xffff then invalid_arg "CBL-08 fixture deflate block is too large" ;
  "\x78\x01\x01" ^ uint16_le length ^ uint16_le (lnot length land 0xffff)
  ^ value ^ uint32_be (adler32 value)

let solid_png red green blue =
  let width = 64 in
  let height = 64 in
  let pixel =
    String.init 3 (function
      | 0 -> Char.chr red
      | 1 -> Char.chr green
      | _ -> Char.chr blue)
  in
  let scanline = "\x00" ^ String.concat "" (List.init width (fun _ -> pixel)) in
  let pixels = String.concat "" (List.init height (fun _ -> scanline)) in
  let ihdr =
    uint32_be (Int32.of_int width)
    ^ uint32_be (Int32.of_int height)
    ^ "\x08\x02\x00\x00\x00"
  in
  "\x89PNG\r\n\x1a\n" ^ png_chunk "IHDR" ihdr
  ^ png_chunk "IDAT" (stored_zlib pixels)
  ^ png_chunk "IEND" ""

(* A deterministic 64x64 solid-red baseline JPEG. Keeping the 336-byte encoded
   image inline avoids an image-library/runtime dependency and a binary fixture. *)
let red_jpeg =
  String.concat ""
    [
      "\xff\xd8\xff\xe0\x00\x10\x4a\x46\x49\x46\x00\x01\x01\x00\x00\x01\x00\x01\x00\x00\xff\xdb\x00\x43";
      "\x00\x02\x01\x01\x01\x01\x01\x02\x01\x01\x01\x02\x02\x02\x02\x02\x04\x03\x02\x02\x02\x02\x05\x04";
      "\x04\x03\x04\x06\x05\x06\x06\x06\x05\x06\x06\x06\x07\x09\x08\x06\x07\x09\x07\x06\x06\x08\x0b\x08";
      "\x09\x0a\x0a\x0a\x0a\x0a\x06\x08\x0b\x0c\x0b\x0a\x0c\x09\x0a\x0a\x0a\xff\xdb\x00\x43\x01\x02\x02";
      "\x02\x02\x02\x02\x05\x03\x03\x05\x0a\x07\x06\x07\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a";
      "\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a";
      "\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\xff\xc0\x00\x11\x08\x00\x40\x00\x40\x03";
      "\x01\x11\x00\x02\x11\x01\x03\x11\x01\xff\xc4\x00\x15\x00\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\x00\x00\x00\x08\xff\xc4\x00\x14\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\x00\x00\xff\xc4\x00\x16\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\x08\x09\xff\xc4\x00\x14\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\xff\xda\x00\x0c\x03\x01\x00\x02\x11\x03\x11\x00\x3f\x00\x98\xd3\xfb\x60\x00\x00";
      "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\xff\xd9";
    ]

let make ~id ~path ~media_type ~response_field ~expected_value bytes =
  let attachment =
    Backend_types.
      {
        id;
        path;
        media_type;
        sha256 = Digestif.SHA256.(to_hex (digest_string bytes));
        size_bytes = String.length bytes;
      }
  in
  {attachment; bytes; response_field; expected_value}

let all =
  [
    make ~id:"fixture-a" ~path:"fixture-a.png" ~media_type:Backend_types.Png
      ~response_field:"png_dominant_color" ~expected_value:"blue"
      (solid_png 20 45 225);
    make ~id:"fixture-b" ~path:"fixture-b.jpg" ~media_type:Backend_types.Jpeg
      ~response_field:"jpeg_dominant_color" ~expected_value:"red" red_jpeg;
  ]

let for_media_types media_types =
  List.filter
    (fun fixture -> List.mem fixture.attachment.media_type media_types)
    all

let color_schema =
  `Assoc
    [
      ("type", `String "string");
      ( "enum",
        `List
          (List.map
             (fun color -> `String color)
             ["black"; "blue"; "green"; "orange"; "purple"; "red"; "white"; "yellow"]) );
    ]

let schema fixtures =
  `Assoc
    [
      ("$schema", `String "https://json-schema.org/draft/2020-12/schema");
      ("type", `String "object");
      ( "properties",
        `Assoc
          (List.map
             (fun fixture -> (fixture.response_field, color_schema))
             fixtures) );
      ( "required",
        `List (List.map (fun fixture -> `String fixture.response_field) fixtures) );
      ("additionalProperties", `Bool false);
    ]

let expected_document fixtures =
  `Assoc
    (List.map
       (fun fixture -> (fixture.response_field, `String fixture.expected_value))
       fixtures)

let expected_document_text fixtures =
  expected_document fixtures |> Yojson.Safe.to_string ~std:true

let expected_members fixtures =
  List.map
    (fun fixture -> (fixture.response_field, fixture.expected_value))
    fixtures

let validate_semantics fixtures document =
  try
    match Yojson.Safe.from_string document with
    | `Assoc members ->
        let actual =
          List.map
            (function name, `String value -> Some (name, value) | _ -> None)
            members
        in
        if List.for_all Option.is_some actual then
          let actual = List.filter_map Fun.id actual |> List.sort compare in
          let expected = expected_members fixtures |> List.sort compare in
          if actual = expected then Ok () else Error Image_semantics_mismatch
        else Error Image_semantics_mismatch
    | _ -> Error Image_semantics_mismatch
  with Yojson.Json_error _ -> Error Image_semantics_mismatch

let validate_response fixtures document =
  match Json_schema_validator.validate ~schema:(schema fixtures) ~document with
  | Error _ -> Error Schema_rejected
  | Ok () -> validate_semantics fixtures document

let write_file path bytes =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel bytes) ;
  Unix.chmod path 0o600

let materialize ~working_dir fixtures =
  List.iter
    (fun fixture ->
      write_file (Filename.concat working_dir fixture.attachment.path) fixture.bytes)
    fixtures ;
  List.map (fun fixture -> fixture.attachment) fixtures

let prompt =
  "Inspect the actual contents of every attached image. Identify the dominant \
   visible color in each image, using one of the color names allowed by the \
   output schema. Return only the JSON object required by the schema. Do not \
   infer colors from attachment identifiers or filenames, and do not include \
   prose or markdown."
